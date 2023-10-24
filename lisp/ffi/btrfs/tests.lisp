;;; src/fs/btrfs/tests.lisp --- BTRFS common-lisp tests

;;; Code:
(defpackage btrfs/tests
  (:use :cl :rt :btrfs))
(in-package :btrfs/tests)

(defsuite :btrfs)
(in-suite :btrfs)
