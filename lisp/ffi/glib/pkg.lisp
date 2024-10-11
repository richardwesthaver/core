;;; pkg.lisp --- Glib FFI

;; 

;;; Code:
(defpackage :glib
  (:use :cl :std :sb-alien)
  (:export :load-glib
           :gmainloop
           :gmaincontext
           :gobjectgroup
           :glist
           :goptioncontext
           :goptiongroup
           :goptionentry))

(in-package :glib)

(define-alien-loader glib "/usr/lib/" "glib-2.0")

(define-alien-type gtype unsigned)

(define-alien-type gtype-interface (* t))

(define-alien-type gdestroy-notify (* t))

(define-opaque gmainloop)
(define-opaque gmaincontext)
(define-opaque gobjectgroup)
(define-opaque glist)

(define-alien-type gthread (* t))
(define-alien-type ghook-list (* t))
(define-alien-type ginitially-unowned (* t))
(define-alien-type gmutex (* t))
(define-alien-type gpointer (* t))

(define-alien-type gcond
    (struct gcond
            (p gpointer)
            (i (array unsigned-int 2))))


(define-alien-type glist-t
  (struct glist
          (data gpointer)
          (next (* glist))
          (prev (* glist))))

(define-alien-type grec-mutex
    (struct grec-mutex
            (p gpointer)
            (i (array unsigned-int 2))))

(define-alien-routine g-main-loop-new (* gmainloop) (context (* gmaincontext)) (is-running boolean))

(define-alien-type gquark (unsigned 32))
(define-alien-type gerror (struct gerror
                                  (domain gquark)
                                  (code int)
                                  (message c-string)))

(define-alien-type goptioncontext (* t))
(define-alien-type goptiongroup (* t))
(define-alien-type goptionentry (* t))

(define-alien-enum (goption-flags int)
                   :none 0
                   :hidden (ash 1 0)
                   :in-main (ash 1 1)
                   :reverse (ash 1 2)
                   :no-arg (ash 1 3)
                   :filename (ash 1 4)
                   :optional-arg (ash 1 5)
                   :noalias (ash 1 6))

(define-alien-enum (goption-arg int)
                   :none 0
                   :string 1
                   :int 2
                   :callback 3
                   :filename 4
                   :string-array 5
                   :filename-array 6
                   :double 7
                   :int64 8)

(define-alien-routine g-option-error-quark gquark)
