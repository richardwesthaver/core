;;; tests.lisp --- Test Package Definitions

;; 

;;; Code:
(in-package :std-user)

(defpkg :core/tests
  (:use :std-lisp :rt :log))

(in-package :core/tests)

(defsuite :core)
