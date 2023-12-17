(in-package :net/tcp)

(defun tcp-server (port)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-bind s #(0 0 0 0) port)
    (loop
          (multiple-value-bind (buf len addr port) (socket-receive s nil 500)
            (format t "Received ~A bytes from ~A:~A - ~A ~%"
                    len addr port (subseq buf 0 (min 10 len)))))))
