;;; dev.lisp --- Event Device FFI

;; 

;;; Code:
(in-package :evdev)

(define-alien-enum (libevdev-read-status unsigned-char)
                   :success 0
                   :sync 1)

(define-alien-routine libevdev-next-event int
  (dev (* libevdev))
  (flags unsigned-int)
  (ev (* input-event)))

(define-alien-routine libevdev-has-event-pending int
  (dev (* libevdev)))

(define-alien-routine libevdev-get-name c-string
  (dev (* libevdev)))

(define-alien-routine libevdev-set-name void
  (dev (* libevdev))
  (name c-string))

(define-alien-routine libevdev-get-phys c-string
  (dev (* libevdev)))

(define-alien-routine libevdev-set-phys void
  (dev (* libevdev))
  (phys c-string))

(define-alien-routine libevdev-get-uniq c-string
  (dev (* libevdev)))

(define-alien-routine libevdev-set-uniq void
  (dev (* libevdev))
  (uniq c-string))

(define-alien-routine libevdev-get-id-product int
  (dev (* libevdev)))

(define-alien-routine libevdev-set-id-product void
  (dev (* libevdev))
  (product-id int))

(define-alien-routine libevdev-get-id-vendor int
  (dev (* libevdev)))

(define-alien-routine libevdev-set-id-vendor void
  (dev (* libevdev))
  (vendor-id int))

(define-alien-routine libevdev-get-id-bustype int
  (dev (* libevdev)))

(define-alien-routine libevdev-set-id-bustype void
  (dev (* libevdev))
  (bustype int))

(define-alien-routine libevdev-get-id-version int
  (dev (* libevdev)))

(define-alien-routine libevdev-set-id-version void
  (dev (* libevdev))
  (version int))

(define-alien-routine libevdev-get-driver-version int
  (dev (* libevdev)))

(define-alien-routine libevdev-has-property int
  (dev (* libevdev))
  (prop unsigned-int))

(define-alien-routine libevdev-enable-property int
  (dev (* libevdev))
  (prop unsigned-int))

(define-alien-routine libevdev-disable-property int
  (dev (* libevdev))
  (prop unsigned-int))

(define-alien-routine libevdev-has-event-type int
  (dev (* libevdev))
  (type unsigned-int))

(define-alien-routine libevdev-has-event-code int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int))

(define-alien-routine libevdev-get-abs-minimum int
  (dev (* libevdev))
  (code unsigned-int))

(define-alien-routine libevdev-get-abs-maximum int
  (dev (* libevdev))
  (code unsigned-int))

(define-alien-routine libevdev-get-abs-fuzz int
  (dev (* libevdev))
  (code unsigned-int))

(define-alien-routine libevdev-get-abs-resolution int
  (dev (* libevdev))
  (code unsigned-int))

(define-alien-routine libevdev-get-abs-info (* input-absinfo)
  (dev (* libevdev))
  (code unsigned-int))

(define-alien-routine libevdev-get-event-value int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int))

(define-alien-routine libevdev-set-event-value int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int)
  (value int))

(define-alien-routine libevdev-fetch-event-value int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int)
  (value (* int)))

(define-alien-routine libevdev-get-slot-value int
  (dev (* libevdev))
  (slot unsigned-int)
  (code unsigned-int))

(define-alien-routine libevdev-set-slot-value int
  (dev (* libevdev))
  (slot unsigned-int)
  (code unsigned-int)
  (value int))

(define-alien-routine libevdev-fetch-slot-value int
  (dev (* libevdev))
  (slot unsigned-int)
  (code unsigned-int)
  (value (* int)))

(define-alien-routine libevdev-get-num-slots int
  (dev (* libevdev)))

(define-alien-routine libevdev-get-current-slot int
  (dev (* libevdev)))

(define-alien-routine libevdev-set-abs-minimum void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(define-alien-routine libevdev-set-abs-maximum void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(define-alien-routine libevdev-set-abs-fuzz void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(define-alien-routine libevdev-set-abs-flat void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(define-alien-routine libevdev-set-abs-resolution void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(define-alien-routine libevdev-set-abs-info void
  (dev (* libevdev))
  (code unsigned-int)
  (abs (* input-absinfo)))

(define-alien-routine libevdev-enable-event-type int
  (dev (* libevdev))
  (type unsigned-int))

(define-alien-routine libevdev-disable-event-type int
  (dev (* libevdev))
  (type unsigned-int))

(define-alien-routine libevdev-enable-event-code int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int)
  (data (* t)))

(define-alien-routine libevdev-disable-event-code int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int))

(define-alien-routine libevdev-kernel-set-abs-info int
  (dev (* libevdev))
  (code unsigned-int)
  (abs (* input-absinfo)))

(define-alien-enum (libevdev-led-value unsigned-char)
                   :on 3
                   :off 4)

(define-alien-routine libevdev-kernel-set-led-value int
  (dev (* libevdev))
  (code unsigned-int)
  (value libevdev-led-value))

;; (define-alien-routine libevdev-kernel-set-led-values int
;;   (dev (* libevdev)))

(define-alien-routine libevdev-set-clock-id int
  (dev (* libevdev))
  (clockid int))

(define-alien-routine libevdev-event-is-type int
  (ev (* input-event))
  (type unsigned-int))

(define-alien-routine libevdev-event-is-code int
  (ev (* input-event))
  (type unsigned-int)
  (code unsigned-int))

(define-alien-routine libevdev-event-type-get-name c-string
  (type unsigned-int))

(define-alien-routine libevdev-event-code-get-name c-string
  (type unsigned-int)
  (code unsigned-int))

(define-alien-routine libevdev-event-value-get-name c-string
  (type unsigned-int)
  (code unsigned-int)
  (value int))

(define-alien-routine libevdev-property-get-name c-string
  (prop unsigned-int))

(define-alien-routine libevdev-event-type-get-max int
  (type unsigned-int))

(define-alien-routine libevdev-event-type-from-name int
  (name c-string))

(define-alien-routine libevdev-event-type-from-name-n int
  (name c-string)
  (len size-t))

(define-alien-routine libevdev-event-code-from-name int
  (type unsigned-int)
  (name c-string))

(define-alien-routine libevdev-event-code-from-name-n int
  (type unsigned-int)
  (name c-string)
  (len size-t))

(define-alien-routine libevdev-event-value-from-name int
  (type unsigned-int)
  (code unsigned-int)
  (name c-string))

(define-alien-routine libevdev-event-type-from-code-name int
  (name c-string))

(define-alien-routine libevdev-event-type-from-code-name-n int
  (name c-string)
  (len size-t))

(define-alien-routine libevdev-event-code-from-code-name int
  (name c-string))

(define-alien-routine libevdev-event-code-from-code-name-n int
  (name c-string)
  (len size-t))

(define-alien-routine libevdev-event-value-from-name-n int
  (type unsigned-int)
  (code unsigned-int)
  (name c-string)
  (len size-t))

(define-alien-routine libevdev-property-from-name int
  (name c-string))

(define-alien-routine libevdev-property-from-name-n int
  (name c-string)
  (len size-t))

(define-alien-routine libevdev-get-repeat int
  (dev (* libevdev))
  (delay (* int))
  (period (* int)))
