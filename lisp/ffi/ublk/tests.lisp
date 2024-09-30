;;; ublk/tests.lisp --- ublk tests

;;; Code:
(defpackage :ublk/tests
  (:use :cl :std :rt :ublk :sb-ext :sb-alien))

(in-package :ublk/tests)

(defsuite :ublk)
(in-suite :ublk)

(load-ublksrv)

(deftest sanity ())
