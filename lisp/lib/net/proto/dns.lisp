;;; lib/net/proto/dns.lisp --- Domain Name Services

;;

;;; Code:
(in-package :net/proto/dns)

(defconstant +dns-port+ 53)
(defconstant +dns-buffer-length+ 4096)

(defvar *cloudflare-servers*
  '("1.1.1.1" "1.0.0.1"))
(defvar *dnswatch-servers*
  '("84.200.69.80" "84.200.70.40"))
(defvar *google-servers*
  '("8.8.8.8" "8.8.4.4"))
(defvar *opendns-servers*
  '("208.67.222.123" "208.67.220.123"))
(defvar *quad9-servers*
  '("9.9.9.9" "149.112.112.112"))

(defvar *dns-servers*
  (list* "127.0.0.1"
         (append *dnswatch-servers* *quad9-servers*
                 *cloudflare-servers* *opendns-servers*
                 *google-servers*)))

(defun try-server (server send send-length recv recv-length &key (attempts 1) (timeout 1))
  (handler-case
      (let ((socket (sb-bsd-sockets:socket-connect server +dns-port+
                                            :protocol :datagram
                                            :element-type '(unsigned-byte 8)
                                            :timeout 1)))
        (unwind-protect
             (loop repeat attempts
                   do (usocket:socket-send socket send send-length)
                      (when (usocket:wait-for-input socket :timeout timeout :ready-only T)
                        (let ((received (nth-value 1 (usocket:socket-receive socket recv recv-length))))
                          (when (and received (< 0 received))
                            (return received)))))
          (usocket:socket-close socket)))
    (usocket:socket-error (e)
      (values NIL e))))

(defmacro with-query-buffer ((send pos hostname type &rest header-args) &body body)
  `(let* ((,send (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))
          (,pos (codec/dns::encode-header ,send 0 :id 42 :recursion-desired T :question-count 1 ,@header-args))
          (,pos (codec/dns::encode-query ,send ,pos ,hostname :type ,type :class 1)))
     (declare (dynamic-extent ,send))
     ,@body))

(defun query (hostname &key (type T) (dns-servers *dns-servers*) (attempts 1) (timeout 1))
  (with-simple-restart (abort "Abort the DNS query.")
    (let ((recv (make-array +dns-buffer-length+ :element-type '(unsigned-byte 8) :initial-element 0)))
      (declare (dynamic-extent recv))
      (with-query-buffer (send send-length hostname type)
        (loop for server in dns-servers
              for recv-length = (try-server server send send-length recv +dns-buffer-length+ :attempts attempts :timeout timeout)
              do (when recv-length
                   (with-simple-restart (continue "Skip this DNS server.")
                     (return (codec/dns::decode-response server recv 0 recv-length))))
              finally (with-simple-restart (continue "Return NIL instead.")
                        (error 'dns-servers-exhausted)))))))

(defun query-data (hostname &rest args &key type dns-servers attempts timeout)
  (declare (ignore dns-servers attempts timeout))
  (loop for record in (getf (apply #'query hostname args) :answers)
        when (eql type (getf record :type))
        collect (getf record :data)))

(defun resolve (hostname &rest args &key type dns-servers attempts timeout)
  (declare (ignore dns-servers attempts timeout))
  (handler-case
      (handler-bind ((dns-server-failure #'continue))
        (let ((list (if type
                        (apply #'query-data hostname args)
                        (append (apply #'query-data hostname :type :A args)
                                (apply #'query-data hostname :type :AAAA args)))))
          (values (first list) list T)))
    (dns-servers-exhausted ()
      (values NIL NIL NIL))))

(defun hostname (ip &rest args &key type dns-servers attempts timeout)
  (declare (ignore type dns-servers attempts timeout))
  (handler-case
      (handler-bind ((dns-server-failure #'continue))
        (let* ((ipv6-p (find #\: ip))
               (parts (if ipv6-p
                          (loop for byte across (usocket:ipv6-host-to-vector ip)
                                collect (format NIL "~x" (ldb (byte 4 4) byte))
                                collect (format NIL "~x" (ldb (byte 4 0) byte)))
                          (split #\. ip)))
               (list (apply #'query-data (format NIL "~{~a.~}~:[in-addr~;ip6~].arpa" (nreverse parts) ipv6-p) :type :PTR args)))
          (values (first list) list T)))
    (dns-condition ()
      (values NIL NIL NIL))))

(define-condition dns-condition ()
  ())

(define-condition dns-server-failure (error dns-condition)
  ((dns-server :initarg :dns-server :reader dns-server)
   (response-code :initarg :response-code :reader response-code))
  (:report (lambda (c s) (format s "DNS server ~%  ~a~%responded with failure code ~d~@[~%  ~a~]"
                                 (dns-server c) (response-code c) (response-code-name (response-code c))))))

(define-condition dns-servers-exhausted (error dns-condition)
  ()
  (:report (lambda (c s) (declare (ignore c)) (format s "All DNS servers failed to provide an answer for the query."))))

(defun response-code-name (code)
  (case code
    (0 :success)
    (1 :format-error)
    (2 :server-failure)
    (3 :no-such-domain)
    (4 :not-implemented)
    (5 :query-refused)
    (6 :name-should-not-exist)
    (7 :set-should-not-exist)
    (8 :set-does-not-exist)
    (9 :not-authorized)
    (10 :not-in-zone)
    (11 :type-not-implemented)
    (16 :bad-version)
    (17 :key-not-recognised)
    (18 :bad-time)
    (19 :bad-mode)
    (20 :duplicate-key)
    (21 :bad-algorithm)
    (22 :bad-truncation)
    (23 :bad-cookie)))

(defmacro with-dns-error-handling (&body body)
  `(handler-bind ((dns-server-failure
                    (lambda (e)
                      (unless (find (response-code e) '(1 3 6 7 8))
                        (continue e)))))
     ,@body))

;;; Note: we assume that we never cross byte boundaries when accessing bits.
(defmacro with-decoding ((octets start &optional (pos (gensym "POS"))) &body body)
  `(let ((,pos ,start))
     (flet ((int1 ()
              (prog1 (logbitp (* 8 (rem ,pos 1)) (aref ,octets (floor ,pos)))
                (incf ,pos 1/8)))
            (int4 ()
              (prog1 (ldb (byte 4 (* 8 (rem ,pos 1))) (aref ,octets (floor ,pos)))
                (incf ,pos 4/8)))
            (int8 ()
              (prog1 (aref ,octets (floor ,pos))
                (incf ,pos 1)))
            (int16 () ;; big-endian
              (prog1 (+ (ash (aref ,octets (+ 0 (floor ,pos))) 8)
                        (ash (aref ,octets (+ 1 (floor ,pos))) 0))
                (incf ,pos 2)))
            (int32 ()
              (prog1 (+ (ash (aref ,octets (+ 0 (floor ,pos))) 24)
                        (ash (aref ,octets (+ 1 (floor ,pos))) 16)
                        (ash (aref ,octets (+ 2 (floor ,pos))) 8)
                        (ash (aref ,octets (+ 3 (floor ,pos))) 0))
                (incf ,pos 4))))
       (declare (ignorable #'int1 #'int4 #'int8 #'int16 #'int32))
       ,@body)))

(defmacro with-encoding ((octets start &optional (pos (gensym "POS"))) &body body)
  `(let ((,pos ,start))
     (flet ((int1 (value)
              (let ((octet (aref ,octets (floor ,pos))))
                (setf (ldb (byte 1 (* 8 (rem ,pos 1))) octet)
                      (ecase value
                        ((0 1) value)
                        ((T) 1)
                        ((NIL) 0)))
                (setf (aref ,octets (floor ,pos)) octet)
                (incf ,pos 1/8)))
            (int4 (value)
              (let ((octet (aref ,octets (floor ,pos))))
                (setf (ldb (byte 4 (* 8 (rem ,pos 1))) octet) value)
                (setf (aref ,octets (floor ,pos)) octet)
                (incf ,pos 4/8)))
            (int8 (value)
              (setf (aref ,octets (floor ,pos)) value)
              (incf ,pos 1))
            (int16 (value) ;; big-endian
              (setf (aref ,octets (+ 0 ,pos)) (ldb (byte 8 8) value))
              (setf (aref ,octets (+ 1 ,pos)) (ldb (byte 8 0) value))
              (incf ,pos 2))
            (int32 (value) ;; big-endian
              (setf (aref ,octets (+ 0 ,pos)) (ldb (byte 8 24) value))
              (setf (aref ,octets (+ 1 ,pos)) (ldb (byte 8 16) value))
              (setf (aref ,octets (+ 2 ,pos)) (ldb (byte 8 8) value))
              (setf (aref ,octets (+ 3 ,pos)) (ldb (byte 8 0) value))
              (incf ,pos 4)))
       (declare (ignorable #'int1 #'int4 #'int8 #'int16 #'int32))
       ,@body)))

(defmacro maybe-set ((octets offset) &body calls)
  `(with-encoding (,octets ,offset pos)
     ,@(loop for (func value) in calls
             collect `(if ,value
                          (,func ,value)
                          (incf pos ,(ecase func
                                       (int1 1/8)
                                       (int4 4/8)
                                       (int8 1)
                                       (int16 2)
                                       (int32 4)))))
     pos))

(defun split (on string)
  (let ((parts ())
        (buffer (make-string-output-stream)))
    (flet ((finish ()
             (let ((buffer (get-output-stream-string buffer)))
               (push buffer parts))))
      (loop for char across string
            do (if (char= on char)
                   (finish)
                   (write-char char buffer))
            finally (finish)))
    (nreverse parts)))
