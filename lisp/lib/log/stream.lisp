;;; log/stream.lisp --- Logging streams

;;; Code:
(in-package :log)

;; from hunchentoot
(defmacro with-log-stream ((stream-var destination &optional (lock (make-mutex :name "log-stream"))) &body body)
  "Bind STREAM-VAR to a regular logging stream for the duration of BODY.

DESTINATION may be either a pathname-designator, a symbol bound to an open
stream, or NIL if logging is ignored.

LOCK refers to the lock that should be held during the logging operation."
  (once-only (destination)
    (let ((body body))
      `(when ,destination
         (with-mutex (,lock)
           (etypecase ,destination
             ((or string pathname)
              (with-open-file (,stream-var ,destination
                                           :direction :output
                                           :element-type 'octet
                                           :if-does-not-exist :create
                                           :if-exists :append)
                ,@body))
             (stream
              (with-open-stream (,stream-var ,destination)
                ,@body))))))))

(defmacro with-fast-log-stream ((stream-var destination &optional (lock (make-mutex :name "log-stream"))) &body body)
  "Bind STREAM-VAR to a 'fast' logging stream for the duration of BODY.

DESTINATION may be either a pathname-designator, a symbol bound to an open
stream, or NIL if logging is ignored.

LOCK refers to the lock that should be held during the logging operation."
  (with-gensyms (binary-stream)
    (once-only (destination)
      (let ((body body))
        `(when ,destination
           (with-mutex (,lock)
             (etypecase ,destination
               ((or string pathname)
                (with-open-file (,binary-stream ,destination
                                                :direction :output
                                                :element-type 'octet
                                                :if-does-not-exist :create
                                                :if-exists :append)
                  (io/fast:with-fast-output (,stream-var ,binary-stream)
                    ,@body)))
               (stream
                (io/fast:with-fast-output (,stream-var ,destination)
                  ,@body)))))))))
