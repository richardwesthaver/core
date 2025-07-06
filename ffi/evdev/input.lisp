;;; input.lisp --- Linux Input drivers

;; https://www.kernel.org/doc/Documentation/input/input.txt

;;; Code:
(in-package :evdev/input)

;; (defun eviocgbit (ev len)
;;   ;; ioctl read
;;   (ioctl 8 2 "E" (+ #x20 ev) len))

(define-alien-type input-event
    (struct input-event
            (time sb-posix::alien-timeval)
            (type (unsigned 16))
            (code (unsigned 16))
            (value (signed 32))))

(define-alien-type input-id
    (struct input-id
            (bustype (unsigned 16))
            (vendor (unsigned 16))
            (product (unsigned 16))
            (version (unsigned 16))))

(define-alien-type input-absinfo
    (struct input-absinfo
            (value signed)
            (minimum signed)
            (maximum signed)
            (fuzz signed)
            (flat signed)
            (resolution signed)))

(define-alien-type input-keymap-entry
    (struct input-keymap-entry
            (flags unsigned-char)
            (len unsigned-char)
            (index unsigned-short)
            (keycode unsigned)
            (scancode (array (unsigned 8) 32))))

(define-alien-type input-mask
    (struct input-mask
            (type unsigned)
            (codes-size unsigned)
            (codes-ptr (unsigned 64))))

(define-alien-type ff-replay
    (struct ff-replay
            (length unsigned-short)
            (delay unsigned-short)))

(define-alien-type ff-trigger
    (struct ff-trigger
            (button unsigned-short)
            (interval unsigned-short)))

(define-alien-type ff-envelope
    (struct ff-envelope
            (attack-length unsigned-short)
            (attack-level unsigned-short)
            (fade-length unsigned-short)
            (fade-level unsigned-short)))

(define-alien-type ff-constant-effect
    (struct ff-constant-effect
            (level short)
            (envelop ff-envelope)))

(define-alien-type ff-ramp-effect
    (struct ff-ramp-effect
            (start-level short)
            (end-level short)
            (envelope ff-envelope)))

(define-alien-type ff-condition-effect
    (struct ff-condition-effect
            (right-saturation unsigned-short)
            (left-saturation unsigned-short)
            (right-coeff short)
            (left-coeff short)
            (deadband unsigned-short)
            (center short)))

(define-alien-type ff-periodic-effect
    (struct ff-periodic-effect
            (waveform unsigned-short)
            (period unsigned-short)
            (magnitutde short)
            (offset short)
            (phase unsigned-short)
            (envelope ff-envelope)
            (custom-len unsigned)
            (custom-data (* short))))

(define-alien-type ff-rumble-effect
    (struct ff-rumble-effect
            (strong-magnitutde unsigned-short)
            (weak-magnitude unsigned-short)))

(define-alien-type ff-effect
    (struct ff-effect
            (type unsigned-short)
            (id short)
            (direction unsigned-short)
            (trigger ff-trigger)
            (replay ff-replay)
            (u (sb-alien:union u 
                               (constant ff-constant-effect)
                               (ramp ff-ramp-effect)
                               (periodic ff-periodic-effect)
                               (condition (array ff-condition-effect 2))
                               (rumble ff-rumble-effect)))))
