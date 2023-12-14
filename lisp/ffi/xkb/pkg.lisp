(defpackage :xkb
  (:use :cl :std :sb-alien)
  (:export 
   :load-xkbcommon
   :xkb-context :xkb-keymap
   :xkb-keycode :xkb-keysym
   :xkb-layout-index :xkb-layout-mask
   :xkb-level-index :xkb-mod-index
   :xkb-mod-mask :xkb-led-index :xkb-led-mask
   :xkb-level-invalid :xkb-keycode-max :xkb-keymap-use-original-format 
   :xkb-keysym-max :xkb-keycode-invalid :xkb-mod-invalid :xkb-layout-invalid
   :xkb-led-invalid))

(in-package :xkb)

(defun load-xkbcommon ()
  (unless (member :xkb *features*)
    (sb-alien:load-shared-object "libxkbcommon.so" :dont-save t)
    (push :xkb *features*)))

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
