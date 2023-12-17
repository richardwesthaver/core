(in-package :net/udp)

(defun udp-server (port)
  (let ((s (make-instance 'inet-socket :type :datagram :protocol :udp)))
    (socket-bind s #(0 0 0 0) port)
    (loop
      (multiple-value-bind (buf len address port) (socket-receive s nil 500)
        (format t "Received ~A bytes from ~A:~A - ~A ~%"
                len address port (subseq buf 0 (min 10 len)))))))

(defmacro with-udp-client-and-server (((socket-class &rest common-initargs)
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
