;;; stream.lisp --- SSL Streams

;; 

;;; Code:
(in-package :tls)

(defclass* ssl-stream (wrapped-stream
                      fundamental-binary-input-stream
                      fundamental-binary-output-stream)
  (socket close-callback
   (sap :initform nil :accessor sap)
   (deadline :initform nil :accessor deadline)
   (output-buffer :accessor output-buffer)
   (output :accessor output :initform 0)
   (input-buffer :accessor input-buffer)
   (peeked :accessor peeked :initform nil)))

(defclass* ssl-server-stream (ssl-stream)
  (certificate key))

(defmethod initialize-instance :after ((self ssl-stream)
                                       &key (buffer-size *ssl-buffer-size*)
                                       (input-buffer-size buffer-size)
                                       (output-buffer-size buffer-size)
                                       &allow-other-keys)
  (setf (output-buffer self) (make-static-vector output-buffer-size)
        (input-buffer self) (make-static-vector input-buffer-size)))

(defmethod print-object ((self ssl-stream) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "for ~A" (ssl-stream-socket self))))

(defmethod stream-element-type ((stream ssl-stream)) '(unsigned-byte 8))

(defmethod close ((self ssl-stream) &key abort)
  (cond
    ((sap self)
     (unless abort
       (force-output self)
       (openssl::ssl-shutdown (sap self))
       (setf (sap self) (ssl-free (sap self)))
       (when (streamp (ssl-stream-socket self))
         (close (ssl-stream-socket self) :abort abort))
       (when-let ((f (ssl-stream-close-callback self)))
         (funcall f)))
       t)
    (t nil)))

(defmethod open-stream-p ((self ssl-stream))
  (sap self))

(defmethod stream-listen ((self ssl-stream))
  (or (peeked self)
      (setf (peeked self)
            (let* ((buf (input-buffer self))
                   (sap (sap self))
                   (*bio-blockp* nil)
                   (n (with-vector-sap (ptr buf)
                        (nonblocking-ssl-funcall self #'plusp #'ssl-read sap ptr 1))))
              (and (> n 0) (elt buf 0))))))

(defmethod stream-read-byte ((stream ssl-stream))
  (or (prog1
          (peeked stream)
        (setf (peeked stream) nil))
      (handler-case
          (let ((buf (input-buffer stream))
                (handle (sap stream)))
            (with-vector-sap (ptr buf)
              (ensure-ssl-funcall
               stream #'plusp #'ssl-read handle ptr 1))
            (elt buf 0))
        (openssl-error-zero-return ()     ;SSL_read returns 0 on end-of-file
          :eof))))

(defmethod stream-read-sequence ((stream ssl-stream) seq &optional start end)
  (when (and (< start end) (peeked stream))
    (setf (elt seq start) (peeked stream))
    (setf (peeked stream) nil)
    (incf start))
  (let ((buf (input-buffer stream))
        (handle (sap stream)))
    (loop
       for length = (min (- end start) (length buf))
       while (plusp length)
       do
         (handler-case
             (let ((read-bytes
                    (with-vector-sap (ptr buf)
                      (ensure-ssl-funcall
                       stream #'plusp #'ssl-read handle ptr length))))
               (replace seq buf :start1 start :end1 (+ start read-bytes))
               (incf start read-bytes))
           (openssl-error-zero-return ()   ;SSL_read returns 0 on end-of-file
             (return))))
    ;; fixme: kein out-of-file wenn (zerop start)?
    start))

(defmethod stream-write-byte ((stream ssl-stream) b)
  (let ((buf (output-buffer stream)))
    (when (eql (length buf) (output stream))
      (force-output stream))
    (setf (elt buf (output stream)) b)
    (incf (output stream)))
  b)

(defmacro while (cond &body body)
  `(do () ((not ,cond)) ,@body))

(defmethod stream-write-sequence ((stream ssl-stream) seq &optional start end)
  (let ((buf (output-buffer stream)))
    (when (> (+ (- end start) (output stream)) (length buf))
      ;; not enough space left?  flush buffer.
      (force-output stream)
      ;; still doesn't fit?
      (while (> (- end start) (length buf))
        (replace buf seq :start2 start)
        (incf start (length buf))
        (setf (output stream) (length buf))
        (force-output stream)))
    (replace buf seq
             :start1 (output stream)
             :start2 start
             :end2 end)
    (incf (output stream) (- end start)))
  seq)

(defmethod stream-finish-output ((stream ssl-stream))
  (stream-force-output stream))

(defmethod stream-force-output ((stream ssl-stream))
  (let ((buf (output-buffer stream))
        (fill-ptr (output stream))
        (handle (sap stream)))
    (when (plusp fill-ptr)
      (unless handle
        (error "output operation on closed SSL stream"))
      (with-vector-sap (ptr buf)
        (ensure-ssl-funcall stream #'plusp #'ssl-write handle ptr fill-ptr))
      (setf (output stream) 0))))

(defun install-nonblock-flag (fd)
  (sb-posix:fcntl 
   fd
   sb-posix::f-setfl
   (logior (sb-posix:fcntl fd sb-posix::f-getfl)
           sb-posix::o-nonblock)))

(defvar *default-unwrap-stream-p* t
  "Default value for UNWRAP-STREAM-P function parameter.

If true (the default), cl+ssl will try to extract file descriptor
from the given TCP Lisp stream and tell OpenSSL to use a socket BIO
based on that file descriptor;
otherwise use a Lisp BIO wrapping the TCP Lisp stream.")

(defun install-sap-and-bio (stream handle socket unwrap-stream-p)
  (setf (sap stream) handle)
  (when unwrap-stream-p
    (let ((fd (sb-sys:fd-stream-fd socket)))
      (when fd
        (setf socket fd))))
  (etypecase socket
    (integer
     (install-nonblock-flag socket)
     (ssl-set-fd handle socket))
    (stream
     (ssl-set-bio handle (bio-new-lisp) (bio-new-lisp))))

  ;; The below call setting +SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+ mode
  ;; existed since commit 5bd5225.
  ;; It is implemented wrong - ssl-ctx-ctrl expects
  ;; a context as the first parameter, not handle.
  ;; It was lucky to not crush on Linux and Windows,
  ;; untill crash was detedcted on OpenBSD + LibreSSL.
  ;; See https://github.com/cl-plus-ssl/cl-plus-ssl/pull/42.
  ;; We keep this code commented but not removed because
  ;; we don't know what David Lichteblau meant when
  ;; added this - maybe he has some idea?
  ;; (Although modifying global context is a bad
  ;; thing to do for install-sap-and-bio function,
  ;; also we don't see a need for movable buffer -
  ;; we don't repeat calls to ssl functions with
  ;; moved buffer).
  ;;
  ;; (ssl-ctx-ctrl handle
  ;;   +SSL_CTRL_MODE+
  ;;   +SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+
  ;;   nil)

  socket)

(defun install-key-and-cert (handle key certificate)
  (when certificate
    (unless (eql 1 (openssl::ssl-use-certificate-file handle
                                             certificate
                                             openssl::+x509-filetype-pem+))
      (error 'openssl-error-initialize
             :reason (format nil "Can't load certificate ~A" certificate))))
  (when key
    (unless (eql 1 (openssl::ssl-use-privatekey-file handle
                                            key
                                            openssl::+x509-filetype-pem+))
      (error 'openssl-error-initialize :reason (format nil "Can't load private key file ~A" key)))))

(defun x509-certificate-names (x509-certificate)
  (unless (null-alien x509-certificate)
    (with-foreign-pointer (buf 1024)
      (let ((issuer-name (x509-get-issuer-name x509-certificate))
            (subject-name (x509-get-subject-name x509-certificate)))
        (values
         (unless (null-alien issuer-name)
           (x509-name-oneline issuer-name buf 1024)
           (sap-alien buf c-string))
         (unless (null-alien subject-name)
           (x509-name-oneline subject-name buf 1024)
           (sap-alien buf c-string)))))))

(defun ssl-stream-x509-certificate (ssl-stream)
  (openssl::ssl-get1-peer-certificate (sap ssl-stream)))

(defun ssl-load-global-verify-locations (&rest pathnames)
  "PATHNAMES is a list of pathnames to PEM files containing server and CA certificates.
Install these certificates to use for verifying on all SSL connections.
After RELOAD, you need to call this again."
  (ensure-ssl)
  (dolist (path pathnames)
    (let ((namestring (namestring (truename path))))
      (with-alien ((cafile c-string namestring))
        (unless (eql 1 (openssl::ssl-ctx-load-verify-locations
                        *ssl-global-context*
                        cafile nil))
          (error "ssl-ctx-load-verify-locations failed."))))))

(defun ssl-set-global-default-verify-paths ()
  "Load the system default verification certificates.
After RELOAD, you need to call this again."
  (ensure-ssl)
  (unless (eql 1 (ssl-ctx-set-default-verify-paths *ssl-global-context*))
    (error "ssl-ctx-set-default-verify-paths failed.")))

(defun maybe-verify-client-stream (ssl-stream verify-mode hostname)
  ;; VERIFY-MODE is one of NIL, :OPTIONAL, :REQUIRED
  ;; HOSTNAME is either NIL or a string.
  (when verify-mode
    (let* ((handle (sap ssl-stream))
           (srv-cert (openssl::ssl-get1-peer-certificate handle)))
      (unwind-protect
           (progn
             (when (and (eq :required verify-mode)
                        (null-alien srv-cert))
               (error 'server-certificate-missing
                      :format-control "The server didn't present a certificate."))
             (let ((err (openssl::ssl-get-verify-result handle)))
               (unless (eql err openssl::+x509-v-ok+)
                 (error 'openssl-error-verify :stream ssl-stream :error-code err)))
             (when (and hostname
                        (not (null-alien srv-cert)))
               (or (verify-hostname srv-cert hostname)
                   ;; verify-hostname must either return true
                   ;; or signal an error
                   (error "Unexpected NIL returned by VERIFY-HOSTNAME for ~A"
                          hostname))))
        (unless (null-alien srv-cert)
          (x509-free srv-cert))))))

(defun make-alpn-proto-string (protocols)
  "Convert list of protocol names to the wire-format byte string."
  (with-output-to-string (s)
    (dolist (proto protocols)
      (check-type proto string)
      (write-char (code-char (length proto)) s)
      (write-string proto s))))

(defmacro with-new-ssl ((var) &body body)
  (with-gensyms (ssl)
    `(let* ((,ssl (ssl-new *ssl-global-context*))
            (,var ,ssl))
       (when (null-alien ,ssl)
         (error 'openssl-error-call :message "Unable to create SSL structure" :queue (read-openssl-error-queue)))
       (protect-abort ((ssl-free ,ssl)) ,@body))))

(defun handle-external-format (stream ef)
  (if ef
      (io:make-flex-stream stream :external-format ef)
      stream))

;; fixme: free the context when errors happen in this function
(defun make-ssl-client-stream (socket
                               &key
                                 (unwrap-stream-p *default-unwrap-stream-p*)
                                 hostname
                                 close-callback
                                 external-format
                                 (verify (if (ssl-check-verify-p)
                                             :optional
                                             *make-ssl-client-stream-verify-default*))
                                 alpn-protocols
                                 certificate key password
                                 (cipher-list *default-cipher-list*)
                                 method
                                 (buffer-size *default-buffer-size*)
                                 (input-buffer-size buffer-size)
                                 (output-buffer-size buffer-size))
  "Performs TLS/SSL handshake over the specified SOCKET using
the SSL_connect OpenSSL function and returns a Lisp stream that
uses OpenSSL library to encrypt the output data when sending
it to the socket and to decrypt the input received.

Uses a global SSL_CTX instance, which can be overriden
by WITH-GLOBAL-CONTEXT. (The global SSL_CTX is
passed as a parameter to an internall call of SSL_new.)

    SOCKET - represents the socket to be wrapped into an SSL stream.
        Can be either a Lisp stream (of an implementation-dependent type) for that
        socket, or an integer file descriptor of that socket. If that's a
        stream, it will be closed automatically when the SSL stream
        is closed. Also, on CCL, (CCL:STREAM-DEADLINE SOCKET) will be used
        as a deadline for 'socket BIO' mode.
        See README.md / Usage / Timeouts and Deadlines for more information.
        If that's a file descriptor, it is not closed automatically
        (you can use CLOSE-CALLBACK to arrange for that).

    UNWRAP-STREAM-P - if true, (STREAM-FD SOCKET) will be attempted
        to extract the file descriptor. Otherwise the SOCKET
        is left as is. Anyway, if in result we end up with an integer
        file descriptor, a socket BIO is used; if we end up with a
        stream - Lisp BIO is used. This parameter defaults to
        *DEFAULT-UNWRAP-STREAM-P* which is initalized to true.
        See README.md / Usage for more information on BIO types.

    HOSTNAME if specified, will be sent by client during TLS negotiation,
        according to the Server Name Indication (SNI) extension to the TLS.
        If we connect to a server handling multiple domain names,
        this extension enables such server to choose certificate for the
        right domain. Also the HOSTNAME is used for hostname verification
        (if verification is enabled by VERIFY).

    CLOSE-CALLBACK - a function to be called when the created
        ssl stream is CL:CLOSE'ed. The only argument is this ssl stream.

    EXTERNAL-FORMAT - if NIL (the default), a plain (UNSIGNED-BYTE 8)
        ssl stream is returned. With a non-NIL external-format, a WRAPPED-STREAM
        capable of character I/O will be returned instead, with the specified
        value as its initial external format.

    VERIFY can be specified either as NIL if no check should be performed,
        :OPTIONAL to verify the server's certificate if server presents one or
        :REQUIRED to verify the server's certificate and fail if an invalid
        or no certificate was presented. Defaults to
        *MAKE-SSL-CLIENT-STREAM-VERIFY-DEFAULT* which is initialized
        to :REQUIRED

        The verification includes verifying the HOSTNAME against the server
        ceritificate, using the VERIFY-HOSTNAME function.

        An error is signalled in case of the certificate or hostname
        verification failure.

        Note, the VERIFY logic expects that the global
        SSL_CTX object does not have the SSL_VERIFY_PEER
        flag enabled - the default for the cl+ssl's global SSL_CTX.
        If the current global SSL_CTX object has SSL_VERIFY_PEER enabled,
        the SSL_Connect will perform certificate (but not hostname)
        verification on its own, and an error will be signalled for a
        bad certificate even with :VERIFY NIL.

    ALPN-PROTOCOLS, if specified, should be a list of alpn protocol names,
        such as \"h2\", that will be offered to the server. The protocol
        selected by the server can be retrieved with
        GET-SELECTED-ALPN-PROTOCOL.

    CERTIFICATE is the path to a file containing a PEM-encoded certificate.
        Note, if one certificate will be used for multiple TLS connections,
        it's better to load it into a common SSL_CTX (context) object rather
        than reading it for every new connection.

    KEY is the path to a PEM-encoded private key file of that certificate.

    PASSWORD the password to use for decryptipon of the KEY (if encrypted).

    CIPHER-LIST - If not NIL, must be a string to pass to SSL_set_cipher_list.
        An ERROR is signalled if SSL_CTX_set_cipher_list fails.
        Defaults to *DEFAULT-CIPHER-LIST* which is initialized to NIL.

    METHOD - usually you want to leave the default value. It is used
        to compute the parameter for OpenSSL function SSL_CTX_new when
        creating the global SSL_CTX object for cl+ssl. This parameter only has
        effect on the first call, when the global SSL_CTX is not yet created.
        The default value is TLS_method on OpenSSL > 1.1.0 and SSLv23_method
        for older OpenSSL versions.

    BUFFER-SIZE - default value for both the INPUT-BUFFER-SIZE and
        OUTPUT-BUFFER-SIZE parameters. In turn defaults to the
        *DEFAULT-BUFFER-SIZE* special variable.

    INPUT-BUFFER-SIZE - size of the input buffer of the ssl stream.
        Defaults to the BUFFER-SIZE parameter.

    OUTPUT-BUFFER-SIZE - size of the output buffer of the ssl stream.
        Defaults to the BUFFER-SIZE parameter."
  (ensure-ssl :method method)
  (let ((stream (make-instance 'ssl-stream
                               :socket socket
                               :close-callback close-callback
                               :input-buffer-size input-buffer-size
                               :output-buffer-size output-buffer-size)))
    (with-new-ssl (handle)
      (if hostname
          (with-alien ((chostname c-string hostname))
            (openssl::ssl-ctrl handle #.openssl::+ssl-ctrl-set-tlsext-hostname+ 1 chostname)))
      (when alpn-protocols
        (with-alien ((string c-string (make-alpn-proto-string alpn-protocols)))
          (ssl-set-alpn-protos handle string (1- (length alpn-protocols)))))
      (setf socket (install-sap-and-bio stream handle socket unwrap-stream-p))
      (openssl::ssl-set-connect-state handle)
      (when (and cipher-list
                 (zerop (ssl-set-cipher-list handle cipher-list)))
        (error 'openssl-error-initialize
               :reason
               "Can't set SSL cipher list: SSL_set_cipher_list returned 0"))
      (with-pem-password (password)
        (install-key-and-cert handle key certificate))
      (collecting-verify-error (handle)
        (ensure-ssl-funcall stream #'plusp #'ssl-connect handle))
      (maybe-verify-client-stream stream verify hostname)
      (handle-external-format stream external-format))))

;; fixme: free the context when errors happen in this function
(defun make-ssl-server-stream (socket
                               &key
                                 (unwrap-stream-p *default-unwrap-stream-p*)
                                 close-callback
                                 external-format
                                 certificate key password
                                 (cipher-list *default-cipher-list*)
                                 method
                                 (buffer-size *default-buffer-size*)
                                 (input-buffer-size buffer-size)
                                 (output-buffer-size buffer-size))
  "Performs server-side TLS handshake over the specified SOCKET using
the SSL_accept OpenSSL function and returns a Lisp stream that
uses OpenSSL library to encrypt the output data when sending
it to the socket and to decrypt the input received.

Uses a global SSL_CTX instance, which can be overriden
by WITH-GLOBAL-CONTEXT. (The global SSL_CTX is
passed as a parameter to an internall call of SSL_new.)

All parameters have the same meaning as documented
for MAKE-SSL-CLIENT-STREAM.
"
  (ensure-ssl :method method)
  (let ((stream (make-instance 'ssl-server-stream
                               :socket socket
                               :close-callback close-callback
                               :certificate certificate
                               :key key
                               :input-buffer-size input-buffer-size
                               :output-buffer-size output-buffer-size)))
    (with-new-ssl (handle)
      (setf socket (install-sap-and-bio stream handle socket unwrap-stream-p))
      (openssl::ssl-set-accept-state handle)
      (when (and cipher-list
                 (zerop (ssl-set-cipher-list handle cipher-list)))
        (error 'openssl-error-initialize
               :reason
               "Can't set SSL cipher list: SSL_set_cipher_list returned 0"))
      (with-pem-password (password)
        (install-key-and-cert handle key certificate))
      (collecting-verify-error (handle)
        (ensure-ssl-funcall stream #'plusp #'ssl-accept handle))
      (handle-external-format stream external-format))))

(defun get-selected-alpn-protocol (ssl-stream)
  "A wrapper around SSL_get0_alpn_selected.
Returns the ALPN protocol selected by server, or NIL if none was selected.

SSL-STREAM is the client ssl stream returned by make-ssl-client-stream. "
  (with-alien ((ptr c-string) (len int))
    (ssl-get0-alpn-selected (sap ssl-stream) (addr ptr) (addr len))
    ptr))
