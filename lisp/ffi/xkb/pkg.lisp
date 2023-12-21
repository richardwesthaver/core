;;; ffi/xkb/pkg.lisp --- XKBCommon bindings

;; We use the X Keyboard Configuration Database for keymaps.

;; ref: https://xkbcommon.org/doc/current/
;; https://www.freedesktop.org/wiki/Software/XKeyboardConfig/
;; https://www.x.org/releases/X11R7.6/doc/xorg-docs/input/XKB-Config.html
;; https://people.uleth.ca/~daniel.odonnell/Blog/custom-keyboard-in-linuxx11
;; 

;;; Code:
(defpackage :xkb
  (:use :cl :std :sb-alien)
  (:export 
   :load-xkb
   :xkb-context :xkb-keymap
   :xkb-keycode :xkb-keysym
   :xkb-layout-index :xkb-layout-mask
   :xkb-level-index :xkb-mod-index
   :xkb-mod-mask :xkb-led-index :xkb-led-mask
   :xkb-level-invalid :xkb-keycode-max :xkb-keymap-use-original-format 
   :xkb-keysym-max :xkb-keycode-invalid :xkb-mod-invalid :xkb-layout-invalid
   :xkb-led-invalid))

(in-package :xkb)

(defun load-xkb (&optional save)
  (prog1 (sb-alien:load-shared-object "libxkbcommon.so" :dont-save (not save))
    (unless (member :xkb *features*)
      (push :xkb *features*))))

(define-alien-type xkb-context (struct xkb-context))
(define-alien-type xkb-keymap (struct xkb-keymap))
(define-alien-type xkb-state (struct xkb-state))
(define-opaque xkb-keycode)
(define-opaque xkb-keysym)
(define-opaque xkb-layout-index)
(define-opaque xkb-layout-mask)
(define-opaque xkb-level-index)
(define-opaque xkb-mod-index)
(define-opaque xkb-mod-mask)
(define-opaque xkb-led-index)
(define-opaque xkb-led-mask)
