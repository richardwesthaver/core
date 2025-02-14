;;; js.lisp --- JS Tests

;; 

;;; Code:
(in-package :syn/tests/lang)
(defpackage :syn/tests/lang/js
  (:use :cl :syn/tests/lang :syn/lang/js :syn/ts :std :rt))
(in-package :syn/tests/lang/js)
(defsuite :syn/lang/js)
(in-suite :syn/lang/js)
(deftest js-src ()
  (istype 'cons (parse-file :javascript (asdf:system-relative-pathname :core "../rust/ui/alik/sw.js"))))
