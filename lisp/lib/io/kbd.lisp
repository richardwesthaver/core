;;; kbd.lisp --- Keyboard-based IO

;; Keyboard-like devices and input

;;; Commentary:

;; refs:

;; - https://www.kernel.org/doc/Documentation/input/event-codes.txt

;; - https://github.com/xkbcommon/libxkbcommon/blob/master/tools/interactive-evdev.c

;; - https://gitlab.freedesktop.org/libevdev/libevdev/-/tree/master/tools

;;; Code:
(in-package :io/kbd)
(load-xkbcommon)
(pushnew :kbd *features*)
;;; Vars
(defconstant +evdev-offset+ 8)
(defconstant +long-bit+ (sb-alien:alien-size sb-alien:unsigned-long))

;;; Conditions
(deferror kbd-error () ())
;;; Objects
(defstruct keyboard path state compose-state)

(defun evdev-bit-p (array bit)
  "Array elements should be unsigned-long."
  (let ((idx (/ bit +long-bit+)))
    ;; the literal 1 here is 1LL in C - there is potential to overflow a
    ;; singled long.
    (logand (aref array idx) (ash 1 (mod bit +long-bit+)))))

(defun new-device-from-path (path)
  (with-fd (fd path :flags sb-posix:o-rdonly :close nil)
    (sb-alien:with-alien ((dev (* evdev::libevdev)))
      (let ((ret (evdev:libevdev-new-from-fd fd (sb-alien:addr dev))))
        (if (minusp ret)
            (sb-unix::strerror (abs ret))
            dev)))))

;; evdev::+ev-cnt+ evdev::+key-cnt+
(defun keyboard-device-p (path)
  (with-open-file (st path :element-type 'octet)
    (let ((evbits (make-array evdev::+ev-cnt+))
          (keybits (make-array evdev::+key-cnt+)))
      ;; (sb-posix:ioctl (fd path)
      (read-sequence evbits st)
      (read-sequence keybits st)
      ;; (cons evbits keybits)
      (loop for i from evdev::+key-reserved+ upto evdev::+key-min-interesting+
            if (not (evdev-bit-p keybits i)) do (break)
            else return t))))
      
(defun make-keyboard-from-dev (dev keymap compose-table))

(defun get-keyboards (keymap compose-table &optional (dir "/dev/input"))
  (let ((devices (directory dir)))
    (dolist (dev devices)
      (let ((ret (make-keyboard-from-dev dev keymap compose-table)))
        ret))))

;; (with-open-file (file "/dev/input/event4")
;;   (let ((fd (sb-sys:fd-stream-fd file))
;;         (evbits))))

;; (xkb::xkb-consumed-mode :xkb)

;; (let ((dev (new-device-from-path "/dev/input/event4")))
;;   (unless (evdev::libevdev-has-event-code dev evdev::+ev-key+ evdev::+key-scrollup+)
;;     (println "probably not a mouse:"))
;;   (println
;;    (list 
;;     (evdev::libevdev-get-name dev) 
;;     (evdev::libevdev-get-id-bustype dev) 
;;     (evdev::libevdev-get-id-vendor dev)))
;;   (with-alien ((ev evdev/input:input-event))
;;     (when (evdev::libevdev-has-event-pending dev)
;;       (println "has event pending"))
;;     (assert (zerop (evdev::libevdev-next-event dev (evdev::libevdev-read-flag :normal) (addr ev))))
;;     (with-alien-slots ((* time) type (code evdev/input::code) (value evdev/input::value)) ev
;;       (println (obj/time:unix-to-timestamp (sb-posix::alien-timeval-sec time)))
;;       (println (evdev::libevdev-event-type-get-name type))
;;       (println (evdev::libevdev-event-code-get-name type code))
;;       (println (evdev::libevdev-event-value-get-name type code value)))))

