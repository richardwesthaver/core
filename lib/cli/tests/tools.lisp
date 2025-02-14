;;; tools.lisp --- Tool Tests

;; 

;;; Code:
(in-package :cli/tests)
(in-suite :cli)

(deftest sbcl ()
  (with-sbcl (:noinform t :quit t)
    (print 1)))
