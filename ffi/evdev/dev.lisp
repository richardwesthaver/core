;;; dev.lisp --- Event Device FFI

;; 

;;; Code:
(in-package :evdev)

(define-alien-enum (libevdev-read-status unsigned-char)
                   :success 0
                   :sync 1)

(defar libevdev-next-event int
  (dev (* libevdev))
  (flags unsigned-int)
  (ev (* input-event)))

(defar libevdev-has-event-pending int
  (dev (* libevdev)))

(defar libevdev-get-name c-string
  (dev (* libevdev)))

(defar libevdev-set-name void
  (dev (* libevdev))
  (name c-string))

(defar libevdev-get-phys c-string
  (dev (* libevdev)))

(defar libevdev-set-phys void
  (dev (* libevdev))
  (phys c-string))

(defar libevdev-get-uniq c-string
  (dev (* libevdev)))

(defar libevdev-set-uniq void
  (dev (* libevdev))
  (uniq c-string))

(defar libevdev-get-id-product int
  (dev (* libevdev)))

(defar libevdev-set-id-product void
  (dev (* libevdev))
  (product-id int))

(defar libevdev-get-id-vendor int
  (dev (* libevdev)))

(defar libevdev-set-id-vendor void
  (dev (* libevdev))
  (vendor-id int))

(defar libevdev-get-id-bustype int
  (dev (* libevdev)))

(defar libevdev-set-id-bustype void
  (dev (* libevdev))
  (bustype int))

(defar libevdev-get-id-version int
  (dev (* libevdev)))

(defar libevdev-set-id-version void
  (dev (* libevdev))
  (version int))

(defar libevdev-get-driver-version int
  (dev (* libevdev)))

(defar libevdev-has-property int
  (dev (* libevdev))
  (prop unsigned-int))

(defar libevdev-enable-property int
  (dev (* libevdev))
  (prop unsigned-int))

(defar libevdev-disable-property int
  (dev (* libevdev))
  (prop unsigned-int))

(defar libevdev-has-event-type int
  (dev (* libevdev))
  (type unsigned-int))

(defar libevdev-has-event-code int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int))

(defar libevdev-get-abs-minimum int
  (dev (* libevdev))
  (code unsigned-int))

(defar libevdev-get-abs-maximum int
  (dev (* libevdev))
  (code unsigned-int))

(defar libevdev-get-abs-fuzz int
  (dev (* libevdev))
  (code unsigned-int))

(defar libevdev-get-abs-resolution int
  (dev (* libevdev))
  (code unsigned-int))

(defar libevdev-get-abs-info (* input-absinfo)
  (dev (* libevdev))
  (code unsigned-int))

(defar libevdev-get-event-value int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int))

(defar libevdev-set-event-value int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int)
  (value int))

(defar libevdev-fetch-event-value int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int)
  (value (* int)))

(defar libevdev-get-slot-value int
  (dev (* libevdev))
  (slot unsigned-int)
  (code unsigned-int))

(defar libevdev-set-slot-value int
  (dev (* libevdev))
  (slot unsigned-int)
  (code unsigned-int)
  (value int))

(defar libevdev-fetch-slot-value int
  (dev (* libevdev))
  (slot unsigned-int)
  (code unsigned-int)
  (value (* int)))

(defar libevdev-get-num-slots int
  (dev (* libevdev)))

(defar libevdev-get-current-slot int
  (dev (* libevdev)))

(defar libevdev-set-abs-minimum void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(defar libevdev-set-abs-maximum void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(defar libevdev-set-abs-fuzz void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(defar libevdev-set-abs-flat void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(defar libevdev-set-abs-resolution void
  (dev (* libevdev))
  (code unsigned-int)
  (val int))

(defar libevdev-set-abs-info void
  (dev (* libevdev))
  (code unsigned-int)
  (abs (* input-absinfo)))

(defar libevdev-enable-event-type int
  (dev (* libevdev))
  (type unsigned-int))

(defar libevdev-disable-event-type int
  (dev (* libevdev))
  (type unsigned-int))

(defar libevdev-enable-event-code int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int)
  (data (* t)))

(defar libevdev-disable-event-code int
  (dev (* libevdev))
  (type unsigned-int)
  (code unsigned-int))

(defar libevdev-kernel-set-abs-info int
  (dev (* libevdev))
  (code unsigned-int)
  (abs (* input-absinfo)))

(define-alien-enum (libevdev-led-value unsigned-char)
                   :on 3
                   :off 4)

(defar libevdev-kernel-set-led-value int
  (dev (* libevdev))
  (code unsigned-int)
  (value libevdev-led-value))

;; (defar libevdev-kernel-set-led-values int
;;   (dev (* libevdev)))

(defar libevdev-set-clock-id int
  (dev (* libevdev))
  (clockid int))

(defar libevdev-event-is-type int
  (ev (* input-event))
  (type unsigned-int))

(defar libevdev-event-is-code int
  (ev (* input-event))
  (type unsigned-int)
  (code unsigned-int))

(defar libevdev-event-type-get-name c-string
  (type unsigned-int))

(defar libevdev-event-code-get-name c-string
  (type unsigned-int)
  (code unsigned-int))

(defar libevdev-event-value-get-name c-string
  (type unsigned-int)
  (code unsigned-int)
  (value int))

(defar libevdev-property-get-name c-string
  (prop unsigned-int))

(defar libevdev-event-type-get-max int
  (type unsigned-int))

(defar libevdev-event-type-from-name int
  (name c-string))

(defar libevdev-event-type-from-name-n int
  (name c-string)
  (len size-t))

(defar libevdev-event-code-from-name int
  (type unsigned-int)
  (name c-string))

(defar libevdev-event-code-from-name-n int
  (type unsigned-int)
  (name c-string)
  (len size-t))

(defar libevdev-event-value-from-name int
  (type unsigned-int)
  (code unsigned-int)
  (name c-string))

(defar libevdev-event-type-from-code-name int
  (name c-string))

(defar libevdev-event-type-from-code-name-n int
  (name c-string)
  (len size-t))

(defar libevdev-event-code-from-code-name int
  (name c-string))

(defar libevdev-event-code-from-code-name-n int
  (name c-string)
  (len size-t))

(defar libevdev-event-value-from-name-n int
  (type unsigned-int)
  (code unsigned-int)
  (name c-string)
  (len size-t))

(defar libevdev-property-from-name int
  (name c-string))

(defar libevdev-property-from-name-n int
  (name c-string)
  (len size-t))

(defar libevdev-get-repeat int
  (dev (* libevdev))
  (delay (* int))
  (period (* int)))
