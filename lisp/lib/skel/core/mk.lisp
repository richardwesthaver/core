(defpackage :skel/core/mk
  (:use :cl :std :std/named-readtables :skel/core/obj :skel/core/proto :skel/core/err)
  (:export :$@ :$< :$^))

(in-package :skel/core/mk)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defreadtable :skel/mk
    (:merge :std)))

;;; Auto Vars

;; simplified version of GNU Make Automatic Variables

;; don't need these: $% $? $+ $*

(defmacro def-mk-auto (sym ll &body body))

(def-mk-auto $@ (rule) (sk-rule-target rule))
(def-mk-auto $< (rule) (car (sk-rule-source rule)))
(def-mk-auto $^ (rule) (sk-rule-source rule))

