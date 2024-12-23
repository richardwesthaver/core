;;; readline.asd --- GNU Readline FFI bindings

;; GNU Readline for Lisp REPLs

;;; Commentary:

;; It is important to support a solid shell-in-shell experience in our user
;; applications. While we always have the option to build a more interactive
;; native Lisp REPL solution, GNU Readline is the defacto standard and
;; designed to handle many of the tricky OS-specific bits for us.

;;; Code:
(defpackage :readline.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :readline.sys)

(defsystem :readline
  :depends-on (:std :sb-grovel)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :readline)
               (:file "readline"))
  :in-order-to ((test-op (test-op "readline/tests"))))

(defsystem :readline/tests
  :depends-on (:rt :readline)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :readline)))
