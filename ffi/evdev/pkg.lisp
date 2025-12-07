;;; pkg.lisp --- libevdev FFI

;; 

;;; Commentary:

;; note that we also wrap the linux input subsystem linux/input.h which does
;; not require loading of the libevdev shared library. See input.lisp.

#|
  evdev is the generic input event interface. It passes the events
generated in the kernel straight to the program, with timestamps. The
API is still evolving, but should be usable now. It's described in
section 5. 

  This should be the way for GPM and X to get keyboard and mouse
events. It allows for multihead in X without any specific multihead
kernel support. The event codes are the same on all architectures and
are hardware independent.

  The devices are in /dev/input:

	crw-r--r--   1 root     root      13,  64 Apr  1 10:49 event0
	crw-r--r--   1 root     root      13,  65 Apr  1 10:50 event1
	crw-r--r--   1 root     root      13,  66 Apr  1 10:50 event2
	crw-r--r--   1 root     root      13,  67 Apr  1 10:50 event3
	...

And so on up to event31.
|#

;;; Code:
(defpackage :evdev/input
  (:use :cl :std :sb-alien)
  (:export
   #:input-event
   #:input-absinfo))

(defpackage :evdev
  (:use :cl :std :sb-alien :evdev/input)
  (:export
   :load-evdev
   #:libevdev-new
   #:libevdev-new-from-fd
   :libevdev-read-flag
   #:libevdev-free
   #:libevdev-set-fd
   #:libevdev))

(in-package :evdev)
(define-alien-loader evdev "/usr/lib/")

(define-alien-type libevdev (struct libevdev))

(define-alien-enum (libevdev-read-flag :type unsigned-char)
                   :sync 1
                   :normal 2
                   :force-sync 4
                   :blocking 8)

(defar libevdev-new (* libevdev))

(defar libevdev-new-from-fd int
  (fd int)
  (dev (* (* libevdev))))

(defar libevdev-free void
  (dev (* libevdev)))

(define-alien-enum (libevdev-log-priority :type unsigned-char)
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

(defar libevdev-set-log-function void
  (logfunc libevdev-log-function)
  (data (* t)))

(defar libevdev-set-log-priority void
  (priority libevdev-log-priority))

(defar libevdev-get-log-priority libevdev-log-priority)

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

(defar libevdev-set-device-log-function void
  (dev (* libevdev))
  (logfunc libevdev-device-log-function)
  (priority libevdev-log-priority)
  (data (* t)))

(define-alien-enum (libevdev-grab-mode :type unsigned-char)
                   :grab 3
                   :ungrab 4)

(defar libevdev-grab int
  (dev (* libevdev))
  (grab libevdev-grab-mode))

(defar libevdev-set-fd int
  (dev (* libevdev))
  (fd int))

(defar libevdev-get-fd int
  (dev (* libevdev)))
