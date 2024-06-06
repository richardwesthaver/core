;;; tests.lisp --- Test Package Definitions

;; 

;;; Code:
(in-package :std-user)

(defpkg :core/tests
  (:nicknames :tests)
  (:use :std-lisp :rt :log))

(in-package :tests)

(defsuite :core)
