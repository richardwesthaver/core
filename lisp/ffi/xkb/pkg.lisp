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
   :xkb-keysym-flags :xkb-context-flags
   :xkb-keysym-get-name :xkb-keysym-from-name :xkb-keysym-to-utf8
   :xkb-keysym-to-utf32 :xkb-utf32-to-keysym :xkb-keysym-to-upper
   :xkb-keysym-to-lower :xkb-context-new :xkb-context-set-user-data
   :xkb-context-get-user-data :xkb-context-include-path-append
   :xkb-context-include-path-append-default :xkb-context-include-path-reset-defaults
   :xkb-context-include-path-clear :xkb-context-num-include-paths :xkb-context-include-path-get))

(in-package :xkb)

(defun load-xkb (&optional save)
  (prog1 (sb-alien:load-shared-object "libxkbcommon.so" :dont-save (not save))
    (unless (member :xkb *features*)
      (push :xkb *features*))))

(define-alien-type xkb-context (struct xkb-context))
(define-alien-type xkb-keymap (struct xkb-keymap))
(define-alien-type xkb-state (struct xkb-state))
(define-alien-type xkb-keycode unsigned-int)
(define-alien-type xkb-keysym unsigned-int)
(define-alien-type xkb-layout-index unsigned-int)
(define-alien-type xkb-layout-mask unsigned-int)
(define-alien-type xkb-level-index unsigned-int)
(define-alien-type xkb-mod-index unsigned-int)
(define-alien-type xkb-mod-mask unsigned-int)
(define-alien-type xkb-led-index unsigned-int)
(define-alien-type xkb-led-mask unsigned-int)
(define-alien-type xkb-keysym-flags boolean)
(define-alien-type xkb-context-flags unsigned-char)
(define-alien-type xkb-keymap-compile-flags boolean)
(define-alien-type xkb-keymap-format boolean)

(define-alien-type xkb-rule-names
    (struct xkb-rule-names
            (rules c-string)
            (model c-string)
            (layout c-string)
            (variant c-string)
            (options c-string)))

(define-alien-routine xkb-keysym-get-name int
  (keysym xkb-keysym-flags)
  (buffer c-string)
  (size size-t))

(define-alien-routine xkb-keysym-from-name xkb-keysym
  (name c-string)
  (flags xkb-keysym-flags))

(define-alien-routine xkb-keysym-to-utf8 int
  (keysym xkb-keysym)
  (buffer c-string)
  (size size-t))

(define-alien-routine xkb-keysym-to-utf32 unsigned-int
  (keysym xkb-keysym))

(define-alien-routine xkb-utf32-to-keysym xkb-keysym
  (ucs unsigned-int))

(define-alien-routine xkb-keysym-to-upper xkb-keysym
  (ks xkb-keysym))

(define-alien-routine xkb-keysym-to-lower xkb-keysym
  (ks xkb-keysym))

(define-alien-routine xkb-context-new (* xkb-context)
  (flags xkb-context-flags))

(define-alien-routine xkb-context-set-user-data void
  (context (* xkb-context))
  (user-data (* t)))

(define-alien-routine xkb-context-get-user-data (* t)
  (context (* xkb-context)))

(define-alien-routine xkb-context-include-path-append int
  (context (* xkb-context))
  (path c-string))

(define-alien-routine xkb-context-include-path-append-default int
  (context (* xkb-context)))

(define-alien-routine xkb-context-include-path-reset-defaults int
  (context (* xkb-context)))

(define-alien-routine xkb-context-include-path-clear void
  (context (* xkb-context)))

(define-alien-routine xkb-context-num-include-paths unsigned-int
  (context (* xkb-context)))

(define-alien-routine xkb-context-include-path-get c-string
  (context (* xkb-context)))

;;; NYI Logging

;; https://xkbcommon.org/doc/current/group__logging.html

;;; Keymap Init
(define-alien-routine xkb-keymap-new-from-names (* xkb-keymap)
  (context (* xkb-context))
  (names (* xkb-rule-names))
  (flags xkb-keymap-compile-flags))

(define-alien-routine xkb-keymap-new-from-file (* xkb-keymap)
  (context (* xkb-context))
  (file (* t))
  (fmt xkb-keymap-format)
  (flags xkb-keymap-compile-flags))

(define-alien-routine xkb-keymap-new-from-string (* xkb-keymap)
  (context (* xkb-context))
  (str c-string)
  (fmt xkb-keymap-format)
  (flags xkb-keymap-compile-flags))

(define-alien-routine xkb-keymap-new-from-buffer (* xkb-keymap)
  (context (* xkb-context))
  (buffer c-string)
  (length size-t)
  (fmt xkb-keymap-format)
  (flags xkb-keymap-compile-flags))

;;; Keymap Components

(define-alien-routine xkb-keymap-min-keycode xkb-keycode
  (keymap (* xkb-keymap)))

(define-alien-routine xkb-keymap-max-keycode xkb-keycode
  (keymap (* xkb-keymap)))

(define-alien-routine xkb-keymap-key-for-each void
  (keymap (* xkb-keymap))
  (iter (* t))
  (data (* t)))

(define-alien-routine xkb-keymap-key-get-name c-string
  (keymap (* xkb-keymap))
  (key xkb-keycode))

(define-alien-routine xkb-keymap-num-mods xkb-mod-index
  (keymap (* xkb-keymap)))

(define-alien-routine xkb-keymap-mod-get-name c-string
  (keymap (* xkb-keymap))
  (idx xkb-mod-index))

(define-alien-routine xkb-keymap-get-index xkb-mod-index
  (keymap (* xkb-keymap))
  (name c-string))

(define-alien-routine xkb-keymap-num-layouts xkb-layout-index
  (keymap (* xkb-keymap)))

(define-alien-routine xkb-keymap-layout-get-name c-string
  (keymap (* xkb-keymap))
  (idx xkb-layout-index))

(define-alien-routine xkb-keymap-layout-get-index xkb-layout-index
  (keymap (* xkb-keymap))
  (name c-string))

(define-alien-routine xkb-keymap-num-leds xkb-led-index
  (keymap (* xkb-keymap)))

(define-alien-routine xkb-keymap-led-get-name c-string
  (keymap (* xkb-keymap))
  (idx xkb-led-index))

(define-alien-routine xkb-keymap-led-get-index xkb-led-index
  (keymap (* xkb-keymap))
  (name c-string))

(define-alien-routine xkb-keymap-num-levels-for-key xkb-level-index
  (keymap (* xkb-keymap))
  (key xkb-keycode)
  (layout xkb-layout-index))

(define-alien-routine xkb-keymap-key-get-mods-for-level size-t
  (keymap (* xkb-keymap))
  (key xkb-keycode)
  (layout xkb-layout-index)
  (level xkb-level-index)
  (masks-out (* xkb-mod-mask))
  (masks-size size-t))

(define-alien-routine xkb-keymap-key-get-syms-by-level int
  (keymap (* xkb-keymap))
  (key xkb-keycode)
  (layout xkb-layout-index)
  (level xkb-level-index)
  (syms-out (* (* xkb-keysym))))

(define-alien-routine xkb-keymap-key-repeats int
  (keymap (* xkb-keymap))
  (key xkb-keycode))

;;; TODO Keyboard State

;;; TODO Compose/Dead-keys support
