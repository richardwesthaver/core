;;; blake3.lisp --- low-level bindings to CBLAKE3

;;; Commentary:

;;; Code:
(defpackage :blake3/pkg
  (:nicknames :blake3)
  (:use :cl :std :sb-alien)
  (:export 
   :+blake3-key-len+
   :+blake3-out-len+
   :+blake3-block-len+
   :+blake3-chunk-len+
   :+blake3-max-depth+
   :blake3-version
   :blake3-chunk-state
   :blake3-hasher
   :blake3-hasher-init
   :blake3-hasher-init-keyed
   :blake3-hasher-init-derive-key
   :blake3-hasher-init-derive-key-raw
   :blake3-hasher-update
   :blake3-hasher-finalize
   :blake3-hasher-finalize-seek
   :blake3-hasher-reset))

(in-package :blake3)

(defvar +blake3-key-len+ 32)
(defvar +blake3-out-len+ 32)
(defvar +blake3-block-len+ 64)
(defvar +blake3-chunk-len+ 1024)
(defvar +blake3-max-depth+ 54)

(define-alien-loader blake3 t)

(define-alien-routine blake3-version c-string)

(define-alien-type blake3-chunk-state
    (struct blake3-chunk-state
            (key (array unsigned-int 8))
            (chunk-counter unsigned-long)
            (buf (array (unsigned 8) 64))
            (buf-len (unsigned 8))
            (blocks-compressed (unsigned 8))
            (flags (unsigned 8))))

(define-alien-type blake3-hasher
  (struct blake3-hasher
          (key (array unsigned-int 8))
          (chunk blake3-chunk-state)
          (cv-stack-len (unsigned 8))
          (cv-stack (array (unsigned 8) 1760))))

(define-alien-routine blake3-hasher-init void (self (* blake3-hasher)))

(define-alien-routine blake3-hasher-init-keyed void 
  (self (* blake3-hasher))
  (key (array (unsigned 8) 32)))

(define-alien-routine blake3-hasher-init-derive-key void 
  (self (* blake3-hasher))
  (context (* char)))

(define-alien-routine blake3-hasher-init-derive-key-raw void 
  (self (* blake3-hasher))
  (context (* t))
  (context-len size-t))

(define-alien-routine blake3-hasher-update void
  (self (* blake3-hasher)) 
  (input (* t))
  (input-len size-t))

(define-alien-routine blake3-hasher-finalize void 
  (self (* blake3-hasher)) 
  (out (* (unsigned 8))) 
  (out-len size-t))

(define-alien-routine blake3-hasher-finalize-seek void 
  (self (* blake3-hasher)) 
  (seek (unsigned 64)) 
  (out (* (unsigned 8))) 
  (out-len size-t))

(define-alien-routine blake3-hasher-reset void (self (* blake3-hasher)))
