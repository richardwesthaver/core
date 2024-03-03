;;; gui/wm/x11/stumpwm.lisp --- StumpWM wrappers

;; StumpWM is an excellent X11-based tiling window manager - on most
;; CPU-bound systems running an X Display Server this is our default
;; WM.

;;; Code:
(in-package :x11/stumpwm)

(defvar *default-modules* '())
(defvar *default-prefix-key* (kbd "s-;"))

(defvar *user-map* (make-sparse-keymap))
(defvar *sudo-map* (make-sparse-keymap))
(defvar *nav-map* (make-sparse-keymap))
(defvar *toggle-map* (make-sparse-keymap))
(defvar *edit-map* (make-sparse-keymap))
(defvar *app-map* (make-sparse-keymap))

(defun load-swank (&rest args)
  (apply #'swank-loader:init args))

(defun show-kernel ()
  (let ((ip (run-shell-command "uname -r" t)))
    (substitute #\Space #\Newline ip)))

(defun show-ip-address ()
  (let ((ip (run-shell-command "ip addr show dev wlan0 | grep 'inet ' | awk '{print $2 }'" t)))
    (substitute #\Space #\Newline ip)))

(defun show-battery-charge ()
  (let ((raw-battery (run-shell-command "acpi | cut -d, -f2" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun show-battery-state ()
  (let ((raw-battery (run-shell-command "acpi | cut -d: -f2 | cut -d, -f1" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun show-hostname ()
  (let ((host-name (run-shell-command "cat /etc/hostname" t)))
    (substitute #\Space #\Newline host-name)))

(defun toggle-mode-line-current ()
  (toggle-mode-line (current-screen) (current-head)))

