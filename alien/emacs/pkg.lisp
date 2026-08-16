;;; emacs/pkg.lisp --- Emacs FFI

;;

;;; Code:
(defpkg emacs
  (:use :std-lisp :sb-alien)
  (:export :emacs-function :emacs-value :emacs-env 
   :emacs-runtime :emacs-finalizer
   :emacs-env-32 :+emacs-value-frame-size+
   :canvas))

(in-package :emacs)

(defconstant +emacs-value-frame-size+ 512)

(define-alien-enum (emacs-funcall-exit)
  :return 0
  :signal 1
  :throw 2)

(define-alien-enum (emacs-process-input-result)
  :continue 0
  :quit 1)

(define-alien-type lisp-object (* t))

(define-alien-type emacs-value-tag
    (struct emacs-value-tag (v lisp-object)))

(define-alien-type emacs-value (* emacs-value-tag))

(define-alien-type canvas
    (struct canvas
      (next (* (struct canvas)))
      (data (* (unsigned 32)))
      (refresh (unsigned 32))
      (width int)
      (height int)))

(define-alien-type emacs-value-frame
    (struct emacs-value-frame
      (objects (array emacs-value-tag #.+emacs-value-frame-size+))
      (offset int)
      (next (* (struct emacs-value-frame)))))

(define-alien-type emacs-value-storage
    (struct emacs-value-storage
      (initial emacs-value-frame)
      (current (* emacs-value-frame))))

(define-alien-type emacs-env-private
    (struct emacs-env-private
      (pending-non-local-exit emacs-funcall-exit)
      (non-local-exit-symbol lisp-object)
      (non-local-exit-data lisp-object)
      (storage emacs-value-storage)))

;; ptrdiff-t?
(define-alien-type emacs-env-32
    (struct emacs-env-32
      (size size-t)
      (private-members (* emacs-env-private))
      ;; fns
      (make-global-ref (* t))
      (free-global-ref (* t))
      (non-local-exit-check (* t))
      (non-local-exit-clear (* t))
      (non-local-exit-get (* t))
      (non-local-exit-signal (* t))
      (non-local-exit-throw (* t))
      (make-function (* t))
      (funcall (* t))
      (intern (* t))
      (type-of (* t))
      (is-not-nil (* t))
      (eq (* t))
      (extract-integer (* t))
      (make-integer (* t))
      (extract-float (* t))
      (make-float (* t))
      (copy-string-contents (* t))
      (make-string (* t))
      (make-user-ptr (* t))
      (get-user-ptr (* t))
      (set-user-ptr (* t))
      (get-user-finalizer (* t))
      (set-user-finalizer (* t))
      (vec-get (* t))
      (vec-set (* t))
      (vec-size (* t))
      (should-quit (* t))
      (process-input (* t))
      (extract-time (* t))
      (make-time (* t))
      (extract-big-integer (* t))
      (make-big-integer (* t))
      (set-function-finalizer (* t))
      (open-channel (* t))
      (make-interactive (* t))
      (make-unibyte-string (* t))
      (canvas-data (* t))))

(define-alien-type emacs-env emacs-env-32)

(define-alien-type emacs-runtime-private
  (struct emacs-runtime-private
    (env (* emacs-env))))

(define-alien-type emacs-runtime
  (struct emacs-runtime
    (size size-t)
    (private-members (* (struct emacs-runtime-private)))
    (get-environment (* (function (* emacs-env) (* emacs-runtime))))))

;; (define-alien-type emacs-init-function
;;   (function int (* (struct emacs-runtime))))

(define-alien-type emacs-function
  (function (* emacs-value) (* emacs-env) size-t
    (* emacs-value) (* t)))

(define-alien-type emacs-finalizer
  (function (* t) (* t)))
