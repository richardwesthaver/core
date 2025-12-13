;;; ctx.lisp --- OpenSSL Context

;; 

;;; Code:
(in-package :cry/tls)

(define-condition verify-location-not-found-error (openssl-error)
  ((location :initarg :location))
  (:documentation "Unable to find verify locations")
  (:report (lambda (condition stream)
             (format stream "Unable to find verify location. Path: ~A" (slot-value condition 'location)))))

(defun validate-verify-location (location)
  (handler-case
      (cond
        ((probe-file location)
         (values location t))
        ((probe-directory location)
         (values location nil))
        (t
         (error 'verify-location-not-found-error :location location)))))

(defun add-verify-locations (ssl-ctx locations)
  (dolist (location locations)
    (multiple-value-bind (location isfile)
        (validate-verify-location location)
      (let ((location-ptr (make-alien-string location)))
        (unless (= 1 (openssl::ssl-ctx-load-verify-locations
                      ssl-ctx
                      (when isfile location-ptr)
                      (unless isfile location-ptr)))
          (error 'openssl-error :queue (openssl::read-openssl-error-queue) 
                                :message (format nil "Unable to load verify location ~A" location)))))))

(defun ssl-ctx-set-verify-location (ssl-ctx location)
  (cond
    ((eq :default location)
     (unless (= 1 (ssl-ctx-set-default-verify-paths ssl-ctx))
       (error 'openssl-error-call
              :queue (read-openssl-error-queue)
              :message (format nil "Unable to load default verify paths"))))
     ((eq :default-file location)
      ;; supported since openssl 1.1.0
      (unless (= 1 (ssl-ctx-set-default-verify-file ssl-ctx))
        (error 'openssl-error-call
               :queue (read-openssl-error-queue)
               :message (format nil "Unable to load default verify file"))))
     ((eq :default-dir location)
      ;; supported since openssl 1.1.0
      (unless (= 1 (ssl-ctx-set-default-verify-dir ssl-ctx))
        (error 'openssl-error-call
               :queue (read-openssl-error-queue)
               :message (format nil "Unable to load default verify dir"))))
    ((stringp location)
     (add-verify-locations ssl-ctx (list location)))
    ((pathnamep location)
     (add-verify-locations ssl-ctx (list location)))
    ((and location (listp location))
     (add-verify-locations ssl-ctx location))
    ;; silently allow NIL as location
    (location
     (error "Invalid location ~a" location))))

(defconstant +SSL-CTRL-SET-MIN-PROTO-VERSION+ 123)
(defconstant +SSL-CTRL-SET-MAX-PROTO-VERSION+ 124)

(defun ssl-ctx-set-min-proto-version (ctx version)
  (openssl::ssl-ctx-ctrl ctx +SSL-CTRL-SET-MIN-PROTO-VERSION+ version nil))

(defun ssl-ctx-set-max-proto-version (ctx version)
  (openssl::ssl-ctx-ctrl ctx +SSL-CTRL-SET-MAX-PROTO-VERSION+ version nil))

(defun make-ssl-context (&key (method nil method-supplied-p)
                          disabled-protocols
                          (options (list openssl::+SSL-OP-ALL+))
                          min-proto-version
                          (session-cache-mode openssl::+ssl-sess-cache-server+)
                          (verify-location :default)
                          (verify-depth 100)
                          (verify-mode openssl::+ssl-verify-peer+)
                          verify-callback
                          cipher-list
                          (pem-password-callback 'pem-password-callback)
                          certificate-chain-file
                          private-key-file
                          private-key-password
                          (private-key-file-type openssl::+x509-filetype-pem+))
  "Creates a new SSL_CTX using SSL_CTX_new and initializes it according to
the specified parameters.

After you're done using the context, don't forget to free it using SSL-CTX-FREE.

Exceptions:

    OPENSSL-ERROR-INITIALIZE. When underlying SSL_CTX_new fails.

Keyword arguments:

    METHOD. Specifies which supported SSL/TLS to use.
        If not specified then TLS_method is used on OpenSSL
        versions supporing it (on legacy versions SSLv23_method is used).

    DISABLED-PROTOCOLS. List of +SSL-OP-NO-* constants. Denotes
        disabled SSL/TLS versions.

    OPTIONS. SSL context options list. Defaults to (list +SSL-OP-ALL+)

    SESSION-CACHE-MODE. Enable/Disable session caching.
        Defaults to +SSL-SESS-CACHE-SERVER+

    VERIFY-LOCATION. Location(s) to load CA from.

        Possible values:
            :DEFAULT - SSL_CTX_set_default_verify_paths will be called.
            :DEFAULT-FILE - SSL_CTX_set_default_verify_file will be called. Requires OpenSSL >= 1.1.0.
            :DEFAULT-DIR - SSL_CTX_set_default_verify_dir will be called. Requires OpenSSL >= 1.1.0.
            A STRING or a PATHNAME - will be passed to SSL_CTX_load_verify_locations
                as file or dir argument depending on wether it's really
                a file or a dir. Must exist on the file system and be available.
            A LIST - each value assumed to be either a STRING or a PATHNAME and
                will be passed to SSL_CTX_load_verify_locations as described above.

    VERIFY-DEPTH. Sets the maximum depth for the certificate chain verification
        that shall be allowed for context. Defaults to 100.

    VERIFY-MODE. The mode parameter to SSL_CTX_set_verify.
        Defaults to +VERIFY-PEER+

    VERIFY-CALLBACK. The verify_callback parameter to SSL_CTX_set_verify.
        Please note: if specified, must be a CFFI callback i.e. defined as
        (DEFCALLBACK :INT ((OK :INT) (SSL-CTX :POINTER)) .. ).

    CIPHER-LIST. If specified, must be a string to pass to SSL_CTX_set_cipher_list.
        An ERROR is signalled if SSL_CTX_set_cipher_list fails.

    PEM-PASSWORD-CALLBACK. Sets the default password callback called when
        loading/storing a PEM certificate with encryption.
        Please note: this must be an alien callable defined as:
        (DEFINE-ALIEN-CALLABLE MY-CALLBACK INT ((BUF (* t)) (SIZE INT) (RWFLAG INT) (UNUSED (* T))) .. ).
        Defaults to PEM-PASSWORD-CALLBACK which simply uses password
        provided by WITH-PEM-PASSWORD."
  (ensure-ssl)
  (let ((ssl-ctx (openssl::ssl-ctx-new
                  (if method-supplied-p
                      method
                      (funcall 'tls-method)))))
    (when (null-alien ssl-ctx)
      (error 'ssl-error-initialize :reason "Can't create new SSL-CTX"
                                   :queue (read-openssl-error-queue)))
    (unwind-protect-case 
        () 
        (progn
          (openssl::ssl-ctx-set-options ssl-ctx
                                        (apply #'logior
                                               (append disabled-protocols options)))
          ;; Older OpenSSL versions might not have this SSL_ctrl call.
          ;; Having them error out is a sane default - it's better than to keep
          ;; on running with insecure values.
          ;; People that _have_ to use much too old OpenSSL versions will
          ;; have to call MAKE-SSL-CONTEXT with :MIN-PROTO-VERSION nil.
          ;;
          ;; As an aside: OpenSSL had the "SSL_OP_NO_TLSv1_2" constant since
          ;;   7409d7ad517    2011-04-29 22:56:51 +0000
          ;; so requiring a "new"er OpenSSL to match CL+SSL's defauls shouldn't be a problem.
          (if min-proto-version
              (if (zerop (ssl-ctx-set-min-proto-version ssl-ctx min-proto-version))
                  (error "Couldn't set minimum SSL protocol version!")))
          (ssl-ctx-set-session-cache-mode ssl-ctx session-cache-mode)
          (ssl-ctx-set-verify-location ssl-ctx verify-location)
          (openssl::ssl-ctx-set-verify-depth ssl-ctx verify-depth)
          (openssl::ssl-ctx-set-verify ssl-ctx verify-mode (when verify-callback
                                                               (alien-callable-function verify-callback)))
          (when (and cipher-list
                     (zerop (openssl::ssl-ctx-set-cipher-list ssl-ctx cipher-list)))
            (error 'ssl-error-initialize
                   :reason
                   "Can't set SSL cipher list: SSL_CTX_set_cipher_list returned 0"
                   :queue (read-openssl-error-queue)))
          ;; (let ((pem-pw-cb (alien-sap (alien-callable-function pem-password-callback))))
          ;; (openssl::ssl-ctx-set-default-password-cb ssl-ctx pem-pw-cb))
          (when certificate-chain-file
            (openssl::ssl-ctx-use-certificate-chain-file ssl-ctx certificate-chain-file))
          (when private-key-file
            (with-pem-password (private-key-password)
              (openssl::ssl-ctx-use-privatekey-file ssl-ctx private-key-file private-key-file-type)))
      ssl-ctx)
      (:abort (ssl-ctx-free ssl-ctx)))))


(defun call-with-global-context (ssl-ctx auto-free-p body-fn)
  ;; Ensure initialized, otherwise cl+ssl functions called
  ;; by the BODY-FN may start initialization which
  ;; will override the global context we bind to SSL-CTX.
  ;; (This may happen when the SSL-CTX is created _not_
  ;; by MAKE-SSL-CONTEXT, which ensures initialization by itself)
  ;; https://github.com/cl-plus-ssl/cl-plus-ssl/issues/191
  ;; (ensure-initialized)
  (let* ((*ssl-global-context* ssl-ctx))
    (unwind-protect (funcall body-fn)
      (when auto-free-p
        (ssl-ctx-free ssl-ctx)))))

(defmacro with-global-context ((ssl-ctx &key auto-free-p) &body body)
  "Executes the BODY with *SSL-GLOBAL-CONTEXT* bound to the SSL-CTX.
If AUTO-FREE-P is true the context is freed using SSL-CTX-FREE before exit. "
  `(call-with-global-context ,ssl-ctx ,auto-free-p (lambda () ,@body)))
