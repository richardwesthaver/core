;;; pkg.lisp --- libevdev FFI

;; 

;;; Commentary:

;; note that we also wrap the linux input subsystem linux/input.h which does
;; not require loading of the libevdev shared library. See input.lisp.

;;; Code:
(defpackage :evdev/input
  (:use :cl :std :sb-alien)
  (:export
   #:input-event
   #:input-absinfo))

(defpackage :evdev
  (:use :cl :std :sb-alien :evdev/input)
  (:export))

(in-package :evdev)
(define-alien-loader "evdev" t "/usr/lib/")

(define-alien-type libevdev (struct libevdev))

(define-alien-enum (libevdev-read-flag unsigned-char)
                   :sync 1
                   :normal 2
                   :force-sync 4
                   :blocking 8)

(define-alien-routine libevdev-new (* libevdev))

(define-alien-routine libevdev-new-from-fd int
  (fd int)
  (dev (* (* libevdev))))

(define-alien-routine libevdev-free void
  (dev (* libevdev)))

(define-alien-enum (libevdev-log-priority unsigned-char)
                   :error 10
                   :info 20
                   :debug 30)


;; close enough
(define-alien-type va-list (struct va-list
                                   (gp-offset unsigned-int)
                                   (fp-offset unsigned-int)
                                   (overflow-arg-area (* t))
                                   (reg-save-area (* t))))

(define-alien-type libevdev-log-function 
  (function void
            libevdev-log-priority
            (* t)
            c-string
            int
            c-string
            c-string
            va-list))

(define-alien-routine libevdev-set-log-function void
  (logfunc libevdev-log-function)
  (data (* t)))

(define-alien-routine libevdev-set-log-priority void
  (priority libevdev-log-priority))

(define-alien-routine libevdev-get-log-priority libevdev-log-priority)

(define-alien-type libevdev-device-log-function
  (function void
            (* libevdev)
            libevdev-log-priority
            (* t)
            c-string
            int
            c-string
            c-string
            va-list))

(define-alien-routine libevdev-set-device-log-function void
  (dev (* libevdev))
  (logfunc libevdev-device-log-function)
  (priority libevdev-log-priority)
  (data (* t)))

(define-alien-enum (libevdev-grab-mode unsigned-char)
                   :grab 3
                   :ungrab 4)

(define-alien-routine libevdev-grab int
  (dev (* libevdev))
  (grab libevdev-grab-mode))

(define-alien-routine libevdev-set-fd int
  (dev (* libevdev))
  (fd int))

(define-alien-routine libevdev-get-fd int
  (dev (* libevdev)))
