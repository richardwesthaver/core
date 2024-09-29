;;; kbd.lisp --- Keyboard-based IO

;; Keyboard-like devices and input

;;; Commentary:

;; refs:

;; - https://www.kernel.org/doc/Documentation/input/event-codes.txt

;;; Code:
(in-package :io/kbd)
(load-xkbcommon)

(deferror kbd-error () ())

(defstruct keyboard path state compose-state)

(defconstant +evdev-offset+ 8)

(defun evdev-bit-is-set (array bit))

(defun keyboard-device-p (path))
  ;; (sb-posix:ioctl (fd path)
(defun make-keyboard-from-dev (dev keymap compose-table))

(defun get-keyboards (keymap compose-table &optional (dir "/dev/input"))
  (let ((devices (directory dir)))
    (dolist (dev devices)
      (let ((ret (make-keyboard-from-dev dev keymap compose-table)))
        ret))))

;; (with-open-file (file "/dev/input/event4")
;;   (let ((fd (sb-sys:fd-stream-fd file))
;;         (evbits))))

                         
