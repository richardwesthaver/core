;;; lib/pod/tests.lisp --- Pod tests

;;

;;; Code:
(defpackage :pod/tests
  (:use :cl :rt :pod))

(in-package :pod/tests)
;; (start-podman-server :local t :port 14884)

(defsuite :pod)
(in-suite :pod)
