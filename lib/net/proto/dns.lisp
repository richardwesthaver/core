;;; lib/net/proto/dns.lisp --- Domain Name Services

;;

;;; Code:
(in-package :net/proto/dns)

(define-condition dns-error (dns-condition net-error) ())

(define-condition dns-servers-exhausted (dns-error)
  ()
  (:report (lambda (c s) (declare (ignore c)) (format s "All DNS servers failed to provide an answer for the query."))))

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
  (cons "127.0.0.1"
        (append *dnswatch-servers* *quad9-servers*
                *cloudflare-servers* *opendns-servers*
                *google-servers*)))

(defun try-server (server send send-length recv recv-length &key (attempts 4) (timeout 1))
  (handler-case
      (let ((socket (sb-bsd-sockets:socket-connect
                     (make-instance 'inet-socket
                       :type :datagram :protocol :udp)
                     (make-inet-address server) +dns-port+)))
        (unwind-protect
             (loop repeat attempts
                   do (sb-bsd-sockets:socket-send socket send send-length)
                      (sb-ext:with-timeout timeout
                        (let ((received (nth-value 1 (socket-receive socket recv recv-length))))
                          (when (and received (< 0 received))
                            (return received)))))
          (socket-close socket)))
    (socket-error (e)
      (values nil e))
    (sb-ext:timeout (e)
      (values nil e))))

(defmacro with-query-buffer ((send pos hostname type &rest header-args) &body body)
  `(let* ((,send (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))
          (,pos (encode-header ,send 0 :id 42 :recursion-desired T :question-count 1 ,@header-args))
          (,pos (encode-query ,send ,pos ,hostname :type ,type :class 1)))
     (declare (dynamic-extent ,send))
     ,@body))

(defun dns-query (hostname &key (type T) (dns-servers *dns-servers*) (attempts 8) (timeout 1))
  (with-simple-restart (abort "Abort the DNS query.")
    (let ((recv (make-array +dns-buffer-length+ :element-type '(unsigned-byte 8) :initial-element 0)))
      (declare (dynamic-extent recv))
      (with-query-buffer (send send-length hostname type)
        (loop for server in dns-servers
              for recv-length = (try-server server send send-length recv +dns-buffer-length+ :attempts attempts :timeout timeout)
              do (when recv-length
                   (with-simple-restart (continue "Skip this DNS server.")
                     (return (decode-response server recv 0 recv-length))))
              finally (with-simple-restart (continue "Return NIL instead.")
                        (error 'dns-servers-exhausted)))))))

(defun query-data (hostname &rest args &key type dns-servers (attempts 8) (timeout 1))
  (declare (ignore dns-servers attempts timeout))
  (loop for record in (getf (apply #'dns-query hostname args) :answers)
        when (eql type (getf record :type))
        collect (getf record :data)))

(defun resolve (hostname &rest args &key type dns-servers attempts timeout)
  "Resolve HOSTNAME and return an ip-address as a string. Returns the top
candidate as the first value and all candidates as the second."
  (declare (ignore dns-servers attempts timeout))
  (handler-case
      (handler-bind ((dns-server-failure #'continue))
        (let ((list (if type
                        (apply #'query-data hostname args)
                        (append (apply #'query-data hostname :type :A args)
                                (apply #'query-data hostname :type :AAAA args)))))
          (values (first list) list)))
    (dns-servers-exhausted ()
      (values nil nil))))

(defun hostname (ip &rest args &key type dns-servers attempts timeout)
  "Return the hostname of IP."
  (declare (ignore type dns-servers attempts timeout))
  (handler-case
      (handler-bind ((dns-server-failure #'continue))
        (let* ((ipv6-p (find #\: ip))
               (parts (if ipv6-p
                          (loop for byte across (usocket:ipv6-host-to-vector ip)
                                collect (format nil "~x" (ldb (byte 4 4) byte))
                                collect (format nil "~x" (ldb (byte 4 0) byte)))
                          (ssplit #\. ip)))
               (list (apply #'query-data (format nil "~{~a.~}~:[in-addr~;ip6~].arpa" (nreverse parts) ipv6-p) :type :ptr args)))
          (values (first list) list)))
    (dns-condition ()
      (values nil nil))))
