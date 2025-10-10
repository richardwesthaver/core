;;; util.lisp --- Network Utils

;; 

;;; Code:
(in-package :net/core)

(defvar *localhost* #(127 0 0 1))

;; returns an alien struct pointer, allocated based on input
(defun %sockaddr (&optional sockaddr &rest addr)
  (check-type addr (or null (cons sequence (cons (unsigned-byte 16)))))
  (let ((host (first addr))
        (port (second addr)))
    (when (and host port)
      (ecase (length host)
        (16 (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-in6))))
              (setf (sockint::sockaddr-in6-family sockaddr)
                    sockint::af-inet6
                    (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 0)
                    (ldb (byte 8 8) port)
                    (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 1)
                    (ldb (byte 8 0) port))
              (dotimes (i 4)
                (setf (sb-alien:deref (sockint::sockaddr-in6-flowinfo sockaddr) i) 0))
              (dotimes (i 16)
                (setf (sb-alien:deref (sockint::sockaddr-in6-addr sockaddr) i) (elt host i)))
              (dotimes (i 4)
                (setf (sb-alien:deref (sockint::sockaddr-in6-scope-id sockaddr) i) 0))
              sockaddr))
        (4 (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-in))))
             (let ((in-port (sockint::sockaddr-in-port sockaddr))
                   (in-addr (sockint::sockaddr-in-addr sockaddr)))
               (declare (fixnum port))
               ;; port and host are represented in C as "network-endian" unsigned
               ;; integers of various lengths.  This is stupid.  The value of the
               ;; integer doesn't matter (and will change depending on your
               ;; machine's endianness); what the bind(2) call is interested in
               ;; is the pattern of bytes within that integer.

               ;; We have no truck with such dreadful type punning.  Octets to
               ;; octets, dust to dust.
               (setf (sockint::sockaddr-in-family sockaddr) sockint::af-inet)
               (setf (sb-alien:deref in-port 0) (ldb (byte 8 8) port))
               (setf (sb-alien:deref in-port 1) (ldb (byte 8 0) port))
               (setf (sb-alien:deref in-addr 0) (elt host 0))
               (setf (sb-alien:deref in-addr 1) (elt host 1))
               (setf (sb-alien:deref in-addr 2) (elt host 2))
               (setf (sb-alien:deref in-addr 3) (elt host 3)))
             sockaddr))))))
         
;; from usocket
(defun get-address-by-name (name)
  "Return the address of a host by NAME."
  (multiple-value-bind (host4 host6)
      (sb-bsd-sockets:get-host-by-name name)
    (let ((addr4 (when host4
                   (car (sb-bsd-sockets::host-ent-addresses host4))))
          (addr6 (when host6
                   (car (sb-bsd-sockets::host-ent-addresses host6)))))
      (values addr4 addr6))))

;; from https://github.com/eudoxia0/find-port
(defun port-open-p (port &key (host *localhost*))
  "Determine if a PORT is open on the given HOST."
  (handler-case
      (let ((socket (make-instance 'inet-socket :type :stream)))
        (setf (sockopt-reuse-address socket) t)
        (socket-bind socket host port)
        (socket-close socket))
    (address-in-use-error (condition)
      (declare (ignore condition))
      nil)))

(defun find-port (&key (min 2000) (max 65535) (host *localhost*))
  "Return the first available port in a range of port numbers."
  (loop :for port :from min :to max :when (port-open-p port :host host) :return port))

;; (find-port)
;; (get-address-by-name "localhost")

;;; Macros
(defmacro with-open-socket ((var socket) &body body)
  "Bind SOCKET to VAR and eval BODY followed by calling SOCKET-CLOSE on SOCKET."
  (once-only (socket)
    `(let ((,var ,socket))
       (unwind-protect (when ,var ,@body)
         (when ,var (socket-close ,var))))))

(defmacro with-client-server (((socket-class &rest common-initargs)
                                   (listen-socket-var &rest listen-address)
                                   (client-socket-var &rest client-address)
                                   server-socket-var)
                                      &body body)
  `(let ((,listen-socket-var (make-instance ',socket-class ,@common-initargs))
         (,client-socket-var (make-instance ',socket-class ,@common-initargs))
         (,server-socket-var))
     (unwind-protect
          (progn
            (setf (sockopt-reuse-address ,listen-socket-var) t)
            (socket-bind ,listen-socket-var ,@listen-address)
            (socket-listen ,listen-socket-var 5)
            (socket-connect ,client-socket-var ,@client-address)
            (setf ,server-socket-var (socket-accept ,listen-socket-var))
            ,@body)
       (socket-close ,client-socket-var)
       (socket-close ,listen-socket-var)
       (when ,server-socket-var
         (socket-close ,server-socket-var)))))
