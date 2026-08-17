;;; emacs/pkg.lisp --- Emacs FFI

;;

;;; Code:
(defpkg emacs
  (:use :std-lisp :sb-alien)
  (:export :emacs-function :emacs-value :emacs-env 
   :emacs-runtime :emacs-finalizer
   :emacs-env-32 :+emacs-value-frame-size+
   :canvas :emacs-major-version))
