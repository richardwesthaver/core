;;; readtable.lisp --- Math Syntax and Readtables

;; Standard Algebraic Notation (LR), Tensor Notation

;;; Commentary:

;; We make use of the same LINPARSE implementation as MATLISP, dispatching to
;; our own machinery.

;;; Code:
(in-package :math/syn)

(defparameter *operator-assoc-table* '((* *)
                                       ;; (.* math:.*)
                                       ;; (@ math:@)
                                       ;; (⊗ math:⊗)
                                       (+ +)
                                       (- -)
                                       ;; (\\ math::b\\)
                                       (/ /)
                                       ;; (./ math:./)
                                       (= =)
                                       ;;Not yet implemented.
                                       ; (.= math:=)
                                       ;;No yet implemented
                                       (^ expt)
                                       (.^ expt)
                                       (sin sin) (cos cos) (tan tan) (asin asin) (acos acos) (atan atan) (exp exp) (log log) (expt expt)
                                       (sinh sinh) (cosh cosh) (tanh tanh) (asinh asinh) (acosh acosh) (atanh atanh)
                                       ;; (transpose math:transpose)
                                       ;; (ctranspose math:ctranspose)
                                       ))

(defun op-overload (expr &aux (table *operator-assoc-table*))
  (maptree-eki #'(lambda (tree)
                   (if (listp tree)
                       (cond
                         ((member (car tree) '(+ * progn)) (values (second tree) #'(lambda (f tree) (funcall f tree))))
                         ((= 2 (length tree)) (values `(,(or (second (assoc (car tree) table)) (car tree)) ,@(cdr tree)) t))
                         (t (values tree t)))
                       (values tree t)))
               expr))

(defun ignore-characters (ignore stream)
  (loop for c = (peek-char nil stream t nil t)
        if (member c ignore :test #'char=) 
        do (read-char stream t nil t) 
        else return nil))
;;
(defmacro inlet (&rest body &aux decls)
  (let ((code (maptree-eki #'(lambda (mrk)
                               (if (listp mrk)
                                   (destructuring-case mrk
                                     ((:deflet arg value)
                                      (push arg decls)
                                      (if (listp arg)
                                          (destructuring-case arg
                                            ((list &rest args)
                                             (setf decls (append (reverse args) decls))
                                             (values `(setf (values ,@args) ,value) t))
                                            ((t) (values `(setf ,arg ,value) t)))
                                          (values `(setf ,arg ,value) t)))
                                     ((t &rest args) (declare (ignore args)) (values mrk t)))
                                   (values mrk t)))
                           body)))
    (if (or decls (cdr code))
        `(let (,@decls) ,@code)
        (car code))))

(defun infix-reader (stream subchar arg)
  ;; Read either #I(...) or #I"..."
  (declare (ignore subchar))
  (assert (null arg) nil "given arg where none was required.")
  (ignore-characters *whitespaces* stream)
  (letv* ((iexpr bind (token-reader stream (ecase (read-char stream t nil t) (#\( (cons #\( #\))) (#\[ (cons #\[ #\])))))
          (lexpr (op-overload (parse-with-lexer (list-lexer (nconc (list 'inlet '\() iexpr (list '\)))) *linfix-parser*))))
    (map nil #'(lambda (x) (setf lexpr (subst (second x) (first x) lexpr))) bind)
    lexpr))
;;
(eval-every
  (defparameter *tensor-symbol*
    `((#\D ,(tensor 'cl:double-float))
      (#\Z ,(tensor '(cl:complex cl:double-float)))
      (#\Q ,(tensor 'cl:rational))
      (#\B ,(tensor 'cl:bit)))))

(defun tensor-reader (stream subchar arg)
  (assert (null arg) nil "given arg where none was required.")
  (let ((cl (second (find subchar *tensor-symbol* :key #'car))))
    (ignore-characters *whitespaces* stream)
    (ecase (peek-char nil stream t nil t)
      (#\[ (let ((expr (cdr (infix-reader stream #\I nil))))
             `(tensor::tensor-copy (list ,@expr) ',cl)))
      (#\( (let ((expr (cdr (infix-reader stream #\I nil))))
             `(tensor::zeros (list ,@expr) ',cl))))))

(defun permutation-cycle-reader (stream subchar arg)
  (declare (ignore subchar))
  (assert (null arg) nil "given arg where none was required.")
  (ignore-characters *whitespaces* stream)
  (ecase (peek-char nil stream t nil t)
    (#\[ (let ((expr (cdr (infix-reader stream #\I nil))))
           (with-gensyms (sto)
             `(let ((,sto (mapcar #'(lambda (x) (apply #'tensor::idxv x)) (list ,@expr))))
                (make-instance 'tensor::permutation-cycle :store ,sto)))))))
