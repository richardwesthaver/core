;;; src/fs/bcachefs/tests.lisp --- BCACHEFS common-lisp tests

;;; Code:
(defpackage bcachefs/tests
  (:use :cl :std :rt :bcachefs :sb-alien))
(in-package :bcachefs/tests)

(defsuite :bcachefs)
(in-suite :bcachefs)

(defvar *test-bcachefs-pathname* (directory-path (symbol-name (gensym "/tmp/bcachefs"))))

(deftest sanity ())
