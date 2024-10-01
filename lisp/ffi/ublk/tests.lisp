;;; ublk/tests.lisp --- ublk tests

;;; Commentary:

;; https://github.com/ublk-org/ublksrv/tree/master/tests

;;; Code:
(defpackage :ublk/tests
  (:use :cl :std :rt :ublk :sb-ext :sb-alien))

(in-package :ublk/tests)

(defsuite :ublk)
(in-suite :ublk)

(load-ublksrv)

(deftest sanity ())

