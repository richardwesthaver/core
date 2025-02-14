;;; shell.lisp --- Shell Reader Tests

;; 

;;; Code:
(in-package :cli/tests)
(in-suite :cli)
(in-readtable :shell)
(deftest shell-reader ()
  ;; can't dump functions
  ;; (is #$ls #,*default-pathname-defaults* $#)
)
