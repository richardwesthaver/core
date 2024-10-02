;;; shell.lisp --- Shell Reader Tests

;; 

;;; Code:
(in-package :cli/tests)
(in-suite :cli)

(defparameter *shell-test-fn* #$ls #,*default-pathname-defaults* $#)

(deftest shell-reader ()
  (in-readtable :shell)
  (is (functionp *shell-test-fn*)))
