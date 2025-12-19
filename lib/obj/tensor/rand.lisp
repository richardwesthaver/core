;;; rand.lisp --- Tensor Random Functions

;; 

;;; Code:
(in-package :obj/tensor)

(declaim (ftype (function () double-float) draw-standard-normal draw-standard-exponential))

(definline draw-standard-exponential ()
  "Return a random variable from the Exponential(1) distribution, which has density exp(-x)."
  ;; Adapted from cl-random, originally written by Tamas Papp
  ;; need 1-random, because there is a small but nonzero chance of getting a 0.
  (- (log (- 1d0 (random 1d0)))))

(definline draw-standard-normal ()
  "Draw a random number from N(0,1)."
  ;; Method from Leva (1992).  This is considered much better/faster than the Box-Muller method.
  ;; Adapted from cl-random, originally written by Tamas Papp
  ;; This seems to be just as fast as Marsaglia with storage.
  (with-optimization (:speed 3 :safety 0)
    (loop
       :do (let* ((u (random 1d0))
                  (v (* 1.7156d0 (- (random 1d0) 0.5d0)))
                  (x (- u 0.449871d0))
                  (y (+ (abs v) 0.386595d0))
                  (q (+ (expt x 2) (* y (- (* 0.19600d0 y) (* 0.25472d0 x))))))
             (declare (type double-float u v x y q))
             (unless (and (> q 0.27597d0)
                          (or (> q 0.27846d0)
                              (plusp (+ (expt v 2) (* 4 (expt u 2) (log u))))))
               (return (/ v u)))))))

(definline draw-standard-normal-single ()
  "Draw a random number from N(0,1)."
  ;; Method from Leva (1992).  This is considered much better/faster than the Box-Muller method.
  ;; Adapted from cl-random, originally written by Tamas Papp
  ;; This seems to be just as fast as Marsaglia with storage.
  (with-optimization (:speed 3 :safety 0)
    (loop
       :do (let* ((u (random 1e0))
                  (v (* 1.7156e0 (- (random 1e0) 0.5e0)))
                  (x (- u 0.449871e0))
                  (y (+ (abs v) 0.386595e0))
                  (q (+ (expt x 2) (* y (- (* 0.19600e0 y) (* 0.25472e0 x))))))
             (declare (type single-float u v x y q))
             (unless (and (> q 0.27597e0)
                          (or (> q 0.27846e0)
                              (plusp (+ (expt v 2) (* 4 (expt u 2) (log u))))))
               (return (/ v u)))))))

(declaim (ftype (function () (complex double-float)) draw-standard-normal-marsaglia))
(definline draw-standard-normal-marsaglia ()
  (with-optimization (:speed 3 :safety 0)
    (loop :do
       (let* ((z (complex (1- (random 2d0)) (1- (random 2d0))))
              (s (abs z)))
         (declare (type (complex double-float) z)
                  (type double-float s))
         (when (<= s 1)
           (let ((mult (/ (* -4 (log s)) s)))
             (declare (type double-float mult))
             (return (* z mult))))))))

(defmacro generate-rand (func type clause)
  (let ((clause (etypecase clause (symbol `(,clause)) (cons clause)))
        (func! (intern (concatenate 'string (symbol-name func) "!"))))
    `(eval-every
       (defun ,func! (tensor)
         (declare (type ,(tensor type) tensor))
         (with-optimization (:speed 3 :safety 0)
           (dorefs (idx (dimensions tensor))
                   ((ref tensor :type ,(tensor type)))
                   (setf ref ,clause)))
         tensor)
       (defun ,func (&optional dims)
         (if dims
             (,func! (zeros dims ',(tensor type)))
             ,clause)))))

(macrolet ((generate-rands ((&rest args))
             `(progn
                ,@(mapcar #'(lambda (x) `(generate-rand ,(car x) double-float ,(cadr x))) args))))
  (generate-rands ((randn (draw-standard-normal))
                   (rand (random 1d0))
                   (rande (draw-standard-exponential)))))

(defun randi (&optional dims (arg 2))
  (if dims
      (let ((ret (zeros dims '(double-float))))
        (dorefs (idx (dimensions ret))
            ((ref ret :type #.(tensor 'double-float)))
            (setf ref (coerce (random arg) 'double-float)))
        ret)
      (random arg)))
