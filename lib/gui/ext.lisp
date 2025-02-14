(in-package :gui/ext)

(defmacro defapp (name opts &body body)
  `(defun ,name ,opts
     ,@body))
