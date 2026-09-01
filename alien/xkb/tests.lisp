;;; xkb/tests.lisp --- XKB Tests

;;; Code:
(defpackage :xkb/tests 
  (:use :cl :rt :std :xkb :sb-alien))
(in-package :xkb/tests)
(defsuite :xkb)
(in-suite :xkb)

(load-xkbcommon)

(deftest xkb-basic ()
  (is (= xkb::+xkb-keysym-max+ 536870911))
  (let* ((ctx (xkb:xkb-context-new (xkb-context-flags :no-flags)))
         (map (xkb::xkb-keymap-new-from-names ctx nil (xkb::xkb-keymap-compile-flags :no-flags)))
         (state (xkb::xkb-state-new map)))
    (sb-alien:with-alien ((buf (* unsigned-char) (sb-alien:make-alien unsigned-char 64)))
      (xkb::xkb-keysym-get-name (xkb::xkb-state-key-get-one-sym state 10) buf 64)
      (is (string= "1" (sb-alien:cast buf sb-alien:c-string))))
    (sb-alien:with-alien ((syms (* (* xkb-keysym)) (sb-alien:make-alien (* xkb-keysym) 20)))
      (is (= 1 (xkb::xkb-state-key-get-syms state 10 syms)))
      (is (= 49 (sb-alien:deref (sb-alien:deref syms 0)))))))

(deftest xcompose ()
  (with-xkb-context (ctx)
    (let ((tbl (xkb::xkb-compose-table-new-from-locale ctx "en_US.UTF-8" 0)))
      (isnt (sb-alien:null-alien tbl))
      (with-xkb-compose-state (st tbl)
        (isnt (sb-alien:null-alien st))))))
