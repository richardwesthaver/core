;;; std/list.lisp --- List utils

;;; Code:
(in-package :std/list)

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

(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
argument.")

(define-modify-macro nconcf (&rest lists) nconc
  "Modify-macro for NCONC. Concatenates LISTS to place designated by the first
argument.")

(define-modify-macro unionf (list &rest args) union
  "Modify-macro for UNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place.")

(define-modify-macro nunionf (list &rest args) nunion
  "Modify-macro for NUNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place. May modify
either argument.")

(define-modify-macro reversef () reverse
  "Modify-macro for REVERSE. Copies and reverses the list stored in the given
place and saves back the result into the place.")

(define-modify-macro nreversef () nreverse
  "Modify-macro for NREVERSE. Reverses the list stored in the given place by
destructively modifying it and saves back the result into the place.")

(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(define-modify-macro deletef (item &rest keyword-arguments)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the KEYWORD-ARGUMENTS.")

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

(defun circular-list (&rest elements)
  "Creates a circular list of ELEMENTS."
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(defun circular-list-p (object)
  "Returns true if OBJECT is a circular list, NIL otherwise."
  (and (listp object)
       (do ((fast object (cddr fast))
            (slow (cons (car object) (cdr object)) (cdr slow)))
           (nil)
         (unless (and (consp fast) (listp (cdr fast)))
           (return nil))
         (when (eq fast slow)
           (return t)))))

(defun circular-tree-p (object)
  "Returns true if OBJECT is a circular tree, NIL otherwise."
  (labels ((circularp (object seen)
             (and (consp object)
                  (do ((fast (cons (car object) (cdr object)) (cddr fast))
                       (slow object (cdr slow)))
                      (nil)
                    (when (or (eq fast slow) (member slow seen))
                      (return-from circular-tree-p t))
                    (when (or (not (consp fast)) (not (consp (cdr slow))))
                      (return
                        (do ((tail object (cdr tail)))
                            ((not (consp tail))
                             nil)
                          (let ((elt (car tail)))
                            (circularp elt (cons object seen))))))))))
    (circularp object nil)))

;;; On Lisp
(defun group (source n)
  (declare (fixnum n))
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun flatten (x)
    (labels ((rec (x acc)
               (cond ((null x) acc)
                     #+sbcl
                     ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                     ((atom x) (cons x acc))
                     (t (rec
                         (car x)
                         (rec (cdr x) acc))))))
      (rec x nil))))

;;; cl-bench utils
;; Destructive merge of two sorted lists.
;; From Hansen's MS thesis.
(defun merge! (a b predicate)
  (labels ((merge-loop (r a b)
             (cond ((funcall predicate (car b) (car a))
                    (setf (cdr r) b)
                    (if (null (cdr b))
                        (setf (cdr b) a)
                        (merge-loop b a (cdr b))))
                   (t ; (car a) <= (car b)
                    (setf (cdr r) a)
                    (if (null (cdr a))
                        (setf (cdr a) b)
                        (merge-loop a (cdr a) b))))))
    (cond ((null a) b)
          ((null b) a)
          ((funcall predicate (car b) (car a))
           (if (null (cdr b))
               (setf (cdr b) a)
               (merge-loop b a (cdr b)))
           b)
          (t                           ; (car a) <= (car b)
           (if (null (cdr a))
               (setf (cdr a) b)
               (merge-loop a (cdr a) b))
           a))))

;; Stable sort procedure which copies the input list and then sorts
;; the new list imperatively. Due to Richard O'Keefe; algorithm
;; attributed to D.H.D. Warren.
(defun sort! (seq predicate)
  (labels ((astep (n)
             (cond ((> n 2)
                    (let* ((j (truncate n 2))
                           (a (astep j))
                           (k (- n j))
                           (b (astep k)))
                      (merge! a b predicate)))
                   ((= n 2)
                    (let ((x (car seq))
                          (y (cadr seq))
                          (p seq))
                      (setf seq (cddr seq))
                      (when (funcall predicate y x)
                        (setf (car p) y)
                        (setf (cadr p) x))
                      (setf (cddr p) nil)
                      p))
                   ((= n 1)
                    (let ((p seq))
                      (setf seq (cdr seq))
                      (setf (cdr p) nil)
                      p))
                   (t nil))))
    (astep (length seq))))

;;; EARLY MACROS
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           ,@body)))))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro/g! ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body))))))

(defmacro defun! (name args &body body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defun ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar (lambda (s)
                         `(,s (gensym ,(subseq (symbol-name s)
                                               2))))
                syms)
           ,@body)))))
