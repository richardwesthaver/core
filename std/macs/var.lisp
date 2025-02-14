;;; std/macs/var.lisp --- Dynamic Variable Macros

;;

;;; Code:
(in-package :std/macs)

;; from HUNCHENTOOT
(defmacro defvar-unbound (name &optional (doc-string ""))
  "Convenience macro to declare unbound special variables with a
documentation string."
  `(progn
     (defvar ,name)
     (setf (documentation ',name 'variable) ,doc-string)
     ',name))
