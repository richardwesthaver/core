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
(load-evdev)
(pushnew :kbd *features*)
;;; Vars
(defconstant +evdev-offset+ 8)
(defconstant +long-bit+ (sb-alien:alien-size sb-alien:unsigned-long))

;;; Conditions
(define-condition kbd-error (error) ())
(deferror simple-kbd-error (simple-error kbd-error) () (:auto t))

;;; Objects
(defstruct keyboard 
  path 
  (sap nil :type (or null (alien (* libevdev)))) ;; device
  (state nil)
  (compose-state nil)
  (keymap nil))

(defun evdev-bit-p (array bit)
  "Array elements should be unsigned-long."
  (let ((idx (/ bit +long-bit+)))
    ;; the literal 1 here is 1LL in C - there is potential to overflow a
    ;; singled long.
    (logand (aref array idx) (ash 1 (mod bit +long-bit+)))))

(defun new-device-from-path (path &optional (error t))
  ;; opening FD may fail if the user does not have read permissions. When
  ;; ERROR is non-nil (the default) this signals an error, else we return nil.
  (handler-case
      (with-fd (fd path :flags sb-posix:o-rdonly :close nil)
        (sb-alien:with-alien ((dev (* evdev::libevdev)))
          (let ((ret (evdev:libevdev-new-from-fd fd (sb-alien:addr dev))))
            (if (minusp ret)
                (simple-kbd-error (sb-unix::strerror (abs ret)))
                dev))))
    (error (c) (when error (error c)))))

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
            when (evdev-bit-p keybits i)
            return t))))
      
(defun make-keyboard-from-dev (dev &optional keymap compose-table)
  "Return a KEYBOARD given a device, keymap, and compose table. Keyword argument
ERROR when non-nil (the default) causes an error to be signaled if the device
can't be opened, else returns nil."
  (make-keyboard :sap dev :keymap keymap))

(defun get-keyboards (keymap compose-table &optional (dir "/dev/input"))
  (let ((devices (directory dir)))
    (dolist (dev devices)
      (let ((ret (make-keyboard-from-dev dev keymap compose-table)))
        ret))))

;; (with-open-file (file "/dev/input/event4")
;;   (let ((fd (sb-sys:fd-stream-fd file))
;;         (evbits))))

;; (xkb::xkb-consumed-mode :xkb)

(defun print-device-input-info (path &optional (error t))
  (when-let ((dev (new-device-from-path path error)))
    (when (evdev::libevdev-has-event-code dev evdev::+ev-key+ evdev::+key-scrollup+)
      (println "best-guess: mouse"))
    (list (evdev::libevdev-get-name dev) 
          (evdev::libevdev-get-id-bustype dev) 
          (evdev::libevdev-get-id-vendor dev)
          (evdev::libevdev-get-id-product dev))))

(defun device-read-event (dev)
  (with-alien ((ev evdev/input:input-event))
    (when (evdev::libevdev-has-event-pending dev)
      (println "has event pending")
      (evdev::libevdev-next-event dev (libevdev-read-flag :normal) (addr ev)))
    (with-alien-slots ((* time) type (code evdev/input::code) (value evdev/input::value)) ev
      (list (cons (sb-posix::alien-timeval-sec time) (sb-posix::alien-timeval-usec time))
            type code value))))
