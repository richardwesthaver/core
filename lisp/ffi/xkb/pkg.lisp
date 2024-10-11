;;; ffi/xkb/pkg.lisp --- XKBCommon bindings

;; We use the X Keyboard Configuration Database for keymaps.

;; ref: https://xkbcommon.org/doc/current/
;; https://www.freedesktop.org/wiki/Software/XKeyboardConfig/
;; https://www.x.org/releases/X11R7.6/doc/xorg-docs/input/XKB-Config.html
;; https://people.uleth.ca/~daniel.odonnell/Blog/custom-keyboard-in-linuxx11

;;; Code:
(defpackage :xkb
  (:use :cl :std :sb-alien)
  (:export 
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
   :xkb-context-include-path-clear :xkb-context-num-include-paths :xkb-context-include-path-get
   :load-xkbcommon))

(in-package :xkb)

(define-alien-loader xkbcommon "/usr/lib/")

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

(define-alien-enum (xkb-keysym-flags unsigned-char)
                   :no-flags 0
                   :case-insensitive (ash 1 0))
(define-alien-enum (xkb-context-flags unsigned-char)
                   :no-flags 0
                   :no-default-includes (ash 1 0)
                   :no-environment-names (ash 1 1)
                   :no-secure-getenv (ash 1 2))

(define-alien-enum (xkb-keymap-compile-flags unsigned-char)
                   :no-flags 0)

(define-alien-enum (xkb-keymap-format int)
                   :text-v1 1)

(define-alien-type xkb-rule-names
    (struct xkb-rule-names
            (rules c-string)
            (model c-string)
            (layout c-string)
            (variant c-string)
            (options c-string)))

(define-alien-routine xkb-keysym-get-name int
  (keysym xkb-keysym)
  (buffer (* char))
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

;; (define-alien-routine xkb-context-get-user-data (* t)
;;   (context (* xkb-context)))

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

(define-alien-enum (xkb-log-level int)
                   :critical 10
                   :error 20
                   :warning 30
                   :info 40
                   :debug 50)
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
(define-alien-routine xkb-state-new (* xkb-state) (keymap (* xkb-keymap)))
(define-alien-routine xkb-state-ref (* xkb-state) (state (* xkb-state)))
(define-alien-routine xkb-state-unref void (state (* xkb-state)))
(define-alien-routine xkb-state-get-keymap (* xkb-keymap) (state (* xkb-state)))

(define-alien-enum (xkb-key-direction unsigned-char)
                   :up 0
                   :down 1)

(define-alien-enum (xkb-state-component int)
                   :mods-depressed (ash 1 0)
                   :mods-latched (ash 1 1)
                   :mods-locked (ash 1 2)
                   :mods-effective (ash 1 3)
                   :layout-depressed (ash 1 4)
                   :layout-latched (ash 1 5)
                   :layout-locked (ash 1 6)
                   :layout-effective (ash 1 7)
                   :leds (ash 1 8))

(define-alien-routine xkb-state-update-key xkb-state-component
  (state (* xkb-state))
  (key xkb-keycode)
  (direction xkb-key-direction))

(define-alien-routine xkb-state-update-mask xkb-state-component
  (state (* xkb-state))
  (depressed-mods xkb-mod-mask)
  (latched-mods xkb-mod-mask)
  (locked-mods xkb-mod-mask)
  (depressed-layout xkb-mod-mask)
  (latched-layout xkb-mod-mask)
  (locked-layout xkb-mod-mask))

(define-alien-routine xkb-state-key-get-syms int
  (state (* xkb-state))
  (key xkb-keycode)
  (syms-out (* (* xkb-keysym))))

(define-alien-routine xkb-state-key-get-utf8 int
  (state (* xkb-state))
  (key xkb-keycode)
  (buffer (* char))
  (size size-t))

(define-alien-routine xkb-state-key-get-utf32 int
  (state (* xkb-state))
  (key xkb-keycode))

(define-alien-routine xkb-state-key-get-one-sym xkb-keysym
  (state (* xkb-state))
  (key xkb-keycode))

(define-alien-routine xkb-state-key-get-layout xkb-layout-index
  (state (* xkb-state))
  (key xkb-keycode))

(define-alien-routine xkb-state-key-get-level xkb-level-index
  (state (* xkb-state))
  (key xkb-keycode)
  (layout xkb-layout-index))

(define-alien-enum (xkb-state-match int)
                   :any (ash 1 0)
                   :all (ash 1 1)
                   :non-exclusive (ash 1 16))

(define-alien-routine xkb-state-serialize-mods xkb-mod-mask
  (state (* xkb-state))
  (components xkb-state-component))

(define-alien-routine xkb-state-serialize-layout xkb-layout-index
  (state (* xkb-state))
  (components xkb-state-component))

(define-alien-routine xkb-state-mod-name-is-active int
  (state (* xkb-state))
  (name (* char))
  (type xkb-state-component))

(define-alien-routine xkb-state-mod-names-are-active int
  (state (* xkb-state))
  (type xkb-state-component)
  (match xkb-state-match)
  #+nil ...)

(define-alien-routine xkb-state-mod-index-is-active int
  (state (* xkb-state))
  (idx xkb-mod-index)
  (type xkb-state-component))

(define-alien-routine xkb-state-mod-indices-are-active int
  (state (* xkb-state))
  (type xkb-state-component)
  (match xkb-state-match)
  #+nil ...)

(define-alien-enum (xkb-consumed-mode int)
                   :xkb 0
                   :gtk 1)

(define-alien-routine xkb-state-key-get-consumed-mods2 xkb-mod-mask
  (state (* xkb-state))
  (key xkb-keycode)
  (mode xkb-consumed-mode))

(define-alien-routine xkb-state-key-get-consumed-mods xkb-mod-mask
  (state (* xkb-state))
  (key xkb-keycode))

(define-alien-routine xkb-state-mod-index-is-consumed2 int
  (state (* xkb-state))
  (key xkb-keycode)
  (idx xkb-mod-index)
  (mode xkb-consumed-mode))

(define-alien-routine xkb-state-mod-index-is-consumed int
  (state (* xkb-state))
  (key xkb-keycode)
  (idx xkb-mod-index))

(define-alien-routine xkb-state-mode-mask-remove-consumed xkb-mod-mask
  (state (* xkb-state))
  (key xkb-keycode)
  (mask xkb-mod-mask))

(define-alien-routine xkb-state-layout-name-is-active int
  (state (* xkb-state))
  (name (* char))
  (type xkb-state-component))

(define-alien-routine xkb-state-layout-index-is-active int
  (state (* xkb-state))
  (idx xkb-layout-index)
  (type xkb-state-component))

(define-alien-routine xkb-state-led-name-is-active int
  (state (* xkb-state))
  (name (* char)))

(define-alien-routine xkb-state-led-index-is-active int
  (state (* xkb-state))
  (idx xkb-led-index))

;;; xkbcommon-compose.h
(define-alien-type xkb-compose-table (struct xkb-compose-table))

(define-alien-type xkb-compose-state (struct xkb-compose-state))

(define-alien-enum (xkb-compose-compile-flags unsigned-char)
                   :no-flags 0)

(define-alien-enum (xkb-compose-format unsigned-char)
                   :text-v1 1)

(define-alien-routine xkb-compose-table-new-from-locale (* xkb-compose-table)
  (context (* xkb-context))
  (locale (* char))
  (flags xkb-compose-compile-flags))

(define-alien-routine xkb-compose-table-new-from-file (* xkb-compose-table)
  (file (* t)) ;;FILE
  (locale (* char))
  (format xkb-compose-format)
  (flags xkb-compose-compile-flags))

(define-alien-routine xkb-compose-table-new-from-buffer (* xkb-compose-table)
  (context (* xkb-context))
  (buffer (* char))
  (length size-t)
  (locale (* char))
  (format xkb-compose-format)
  (flags xkb-compose-compile-flags))

(define-alien-routine xkb-compose-table-ref (* xkb-compose-table)
  (table (* xkb-compose-table)))

(define-alien-routine xkb-compose-table-unref void
  (table (* xkb-compose-table)))

(define-alien-type xkb-compose-table-entry (struct xkb-compose-table-entry))

(define-alien-routine xkb-compose-table-entry-sequence (* xkb-keysym)
  (entry (* xkb-compose-table-entry))
  (sequence-length (* size-t)))

(define-alien-routine xkb-compose-table-entry-keysym xkb-keysym
  (entry (* xkb-compose-table-entry)))

(define-alien-routine xkb-compose-table-entry-utf8 (* char)
  (entry (* xkb-compose-table-entry)))

(define-alien-type xkb-compose-table-iterator (struct xkb-compose-table-iterator))

(define-alien-routine xkb-compose-table-iterator-new (* xkb-compose-table-iterator)
  (table (* xkb-compose-table)))

(define-alien-routine xkb-compose-table-iterator-free void
  (iter (* xkb-compose-table-iterator)))

(define-alien-routine xkb-compose-table-iterator-next (* xkb-compose-table-entry)
  (iter (* xkb-compose-table-iterator)))

(define-alien-enum (xkb-compose-state-flags int)
                   :no-flags 0)

(define-alien-routine xkb-compose-state-new (* xkb-compose-state)
  (table (* xkb-compose-table))
  (flags xkb-compose-state-flags))

(define-alien-routine xkb-compose-state-ref (* xkb-compose-state)
  (state (* xkb-compose-state)))

(define-alien-routine xkb-compose-state-unref void
  (state (* xkb-compose-state)))

(define-alien-routine xkb-compose-state-get-compose-table (* xkb-compose-table)
  (state (* xkb-compose-state)))

(define-alien-enum (xkb-compose-status unsigned-char)
                   :nothing 0
                   :composing 1
                   :composed 2
                   :cancelled 3)

(define-alien-enum (xkb-compose-feed-result unsigned-char)
                   :ignored 0
                   :accepted 1)

(define-alien-routine xkb-compose-state-feed xkb-compose-feed-result
  (state (* xkb-compose-state))
  (keysym xkb-keysym))

(define-alien-routine xkb-compose-state-reset void
  (state (* xkb-compose-state)))

(define-alien-routine xkb-compose-state-get-status xkb-compose-status
  (state (* xkb-compose-state)))

(define-alien-routine xkb-compose-state-get-utf8 int
  (state (* xkb-compose-state))
  (buffer (* char))
  (size size-t))

(define-alien-routine xkb-compose-state-get-one-sym xkb-keysym
  (state (* xkb-compose-state)))
