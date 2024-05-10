(in-package :vc)

(defun namestring-or (obj)
  (if (pathnamep obj)
      (namestring obj)
      obj))
