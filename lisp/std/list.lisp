;;; std/list.lisp --- List utils

;;; Code:
(defpackage :std/list
  (:nicknames :list)
  (:use :cl)
  (:export
   #:ensure-car
   #:ensure-cons
   :let-binding-transform))

(in-package :list)

;; (reexport-from :sb-int
;; 	       :include '(:recons :memq :assq :ensure-list :proper-list-of-length-p :proper-list-p
;; 			  :singleton-p))

(defun ensure-car (thing)
  "If THING is a CONS, its CAR is returned. Otherwise THING is returned."
  (if (consp thing)
      (car thing)
      thing))

(defun ensure-cons (cons)
  "If CONS is a cons, it is returned. Otherwise returns a fresh cons with CONS
  in the car, and NIL in the cdr."
  (if (consp cons)
      cons
      (cons cons nil)))

(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
            ((consp (car bs))
              (car bs))
            (t
              (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))
