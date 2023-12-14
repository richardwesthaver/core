;;; k.lisp --- low-level bindings to ngn/k

;;; Commentary:

;;; Code:
(defpackage :k
  (:use :cl :std :sb-alien)
  (:export 
   :load-k
   :K
   :kinit
   :unref
   :c-k :i-k :f-k :l-k :k-a :k-r :t-k
   :ck :n-k :k2f :ik :fk :kc :ks :ki :kf
   :k-c :k-s :k-i :k-f
   :ref :kp :k-e :k-l :k0))

(in-package :k)

(defun load-k () 
  (unless (member :k *features*)
    (sb-alien:load-shared-object "libk.so" :dont-save t)
    (push :k *features*)))

(define-alien-type K (* t))
(define-alien-routine kinit void)
(define-alien-routine unref void (k K))
(define-alien-routine ("CK" c-k) void (c c-string) (k K))
(define-alien-routine ("IK" i-k) void (i (* int)) (k K))
(define-alien-routine ("FK" f-k) void (f (* float)) (k K))
(define-alien-routine ("LK" l-k) void (k0 (* K)) (k1 K))
(define-alien-routine ("KA" k-a) void (q c-string) (k K))
(define-alien-routine ("KR" k-r) K (q c-string) (f (* t)) (i int))
(define-alien-routine ("TK" t-k) char (k K))
(define-alien-routine "cK" char (k K))
(define-alien-routine ("NK" n-k) size-t (k K))
(define-alien-routine "K2f" K (k0 K) (k1 K))
(define-alien-routine "iK" int (k K))
(define-alien-routine "fK" float (k K))
(define-alien-routine "Kc" K (c char))
(define-alien-routine "Ks" K (c c-string))
(define-alien-routine "Ki" K (i int))
(define-alien-routine "Kf" K (f float))
(define-alien-routine ("KC" k-c) K (k (* K)) (i int))
(define-alien-routine ("KS" k-s) K (c (* c-string)) (n size-t))
(define-alien-routine ("KI" k-i) K (i (* int)) (n size-t))
(define-alien-routine ("KF" k-f) K (f (* float)) (n size-t))
(define-alien-routine ref K (k K))
(define-alien-routine "Kp" K (v (* t)))
(define-alien-routine ("KE" k-e) K (v (* t)))
(define-alien-routine ("KL" k-l) K (k (* k)) (n size-t))
(define-alien-routine "K0" K (k (* K)) (c (* char)) (k1 (* K)) (i int))

