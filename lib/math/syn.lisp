;;; readtable.lisp --- Math Syntax and Readtables

;; Standard Algebraic Notation (LR), Tensor Notation

;;; Commentary:

;; We make use of the same LINPARSE implementation as MATLISP, dispatching to
;; our own machinery.

;;; Code:
(in-package :math/syn)

(eval-always
  (defparameter *tensor-symbol*
    '((#\D (tensor 'double-float)
       (#\Z (tensor '(complex double-float)))
       (#\N (tensor 'fixnum))
       (#\B (tensor 'boolean))))))

(defparameter *operator-assoc-table* nil
  #+nil 
  '((* math::tb*-opt)
    (.* math::tb.*)
    (@ math::tb@)
    (^ math::tb^)
    (+ math::tb+)
    (- math::tb-)
    (\\ math::tb\\)
    (/ math::tb/)
    (./ math::tb./)
    (== math::tb==)
    (transpose math::transpose)
    (ctranspose math::ctranspose)))

(defun op-overload (expr)
  (labels ((walker (expr)
             (dwalker
              (cond
                ((atom expr) expr)
                ((and (member (car expr) '(+ * progn)) (not (cddr expr))) (walker (second expr)))
                ((eq (car expr) '*)
                 (if (and (consp (second expr)) (eq (car (second expr)) '/) (not (cddr (second expr)))) ;;ldiv
                     `(\\ (* ,@(cddr expr)) ,(cadr (second expr)))
                     (loop for op in (cdr expr)
                           for lst on (cdr expr)
                           if (and (consp op) (eq (car op) '/) (not (cddr op)))
                           return (walker
                                   (let ((left `(/ (* ,@oplist) ,(second op)) ))
                                     (if (cdr lst)
                                         `(* ,left ,@(cdr lst))
                                         left)))
                           else collect op into oplist
                           finally (return expr))))
                (t expr))))
           (dwalker (expr)
             (if (atom expr) expr
                 (cond
                   ((and (eq (car expr) '/) (not (cddr expr)))
                    `(,(or (second (assoc (car expr) *operator-assoc-table*)) (car expr)) ,(walker (second expr)) nil))
                   (t
                    `(,(or (second (assoc (car expr) *operator-assoc-table*)) (car expr))
                       ,@(mapcar #'walker (cdr expr))))))))
    (walker expr)))

(defun infix-reader (stream subchar arg)
  ;; Read either #I(...) or #I"..."
  (declare (ignore subchar))
  (assert (null arg) nil "given arg where none was required.")
  ;;(ignore-characters +blank-characters+ stream)
  (multiple-value-bind (iexpr bind) (token-reader stream (ecase (read-char stream t nil t) (#\( (cons #\( #\))) (#\[ (cons #\[ #\]))))
    (setf iexpr (nconc (list 'progn '\() iexpr (list '\))))
    (let ((lexpr (op-overload (parse/yacc:parse-with-lexer (list-lexer iexpr) *linfix-parser*))))
      (map nil #'(lambda (x) (setf lexpr (subst (second x) (first x) lexpr))) bind)
      lexpr)))

(defun tensor-reader (stream subchar arg))

(macrolet ((tensor-symbol-enumerate ()
             `(defreadtable :tensor
                (:merge :std)
                (:dispatch-macro-char #\# #\I #'infix-reader)
                ,@(mapcar #'(lambda (x) `(:dispatch-macro-char #\# ,(car x) #'tensor-reader)) *tensor-symbol*))))
  (tensor-symbol-enumerate))

(defreadtable :math
  (:merge :std))
