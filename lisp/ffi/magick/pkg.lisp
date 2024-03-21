;;; magick.lisp --- low-level bindings to CMAGICK

;;; Commentary:

;;; Code:
(defpackage :magick/pkg
  (:nicknames :magick)
  (:use :cl :std :sb-alien)
  (:export ))

(in-package :magick)

(define-alien-loader "magick" t)

(define-alien-routine magick-version c-string)

(define-alien-type magick-chunk-state
    (struct magick-chunk-state
            (key (array unsigned-int 8))
            (chunk-counter unsigned-long)
            (buf (array (unsigned 8) 64))
            (buf-len (unsigned 8))
            (blocks-compressed (unsigned 8))
            (flags (unsigned 8))))

(define-alien-type magick-hasher
    (struct magick-hasher
            (key (array unsigned-int 8))
            (chunk magick-chunk-state)
            (cv-stack-len (unsigned 8))
            (cv-stack (array (unsigned 8) 1760))))

(define-alien-routine magick-hasher-init void (self (* magick-hasher)))

(define-alien-routine magick-hasher-init-keyed void 
  (self (* magick-hasher))
  (key (array (unsigned 8) 32)))

(define-alien-routine magick-hasher-init-derive-key void 
  (self (* magick-hasher))
  (context (* char)))

(define-alien-routine magick-hasher-init-derive-key-raw void 
  (self (* magick-hasher))
  (context (* t))
  (context-len size-t))

(define-alien-routine magick-hasher-update void
  (self (* magick-hasher)) 
  (input (* t))
  (input-len size-t))

(define-alien-routine magick-hasher-finalize void 
  (self (* magick-hasher)) 
  (out (* (unsigned 8))) 
  (out-len size-t))

(define-alien-routine magick-hasher-finalize-seek void 
  (self (* magick-hasher)) 
  (seek (unsigned 64)) 
  (out (* (unsigned 8))) 
  (out-len size-t))

(define-alien-routine magick-hasher-reset void (self (* magick-hasher)))
