;;; memo.lisp --- Simple Memoization

;; 

;;; Code:
(in-package :std/macs)

(defmacro with-memoization ((&optional (hash-table `(make-hash-table :test 'equal))) &body body &aux cache need-hashtablep)
  (with-gensyms (table value exists-p args)
    (labels ((transformer (x)
               (ematch x
                 ((or (list* 'with-memoization _) (list* 'quote _)) x)
                 ((list* 'memoizing body)
                  (match body
                    ((list (lambda-list 'cl:let bindings &body (or (list* (and (list* 'cl:declare _) decl-p) body) body)
                                  &aux (declares (if decl-p (list decl-p))) (id (gensym "memo-"))))
                     (setf need-hashtablep t)
                     `(let (,@bindings)
                        ,@declares
                        (letv* ((,args (list ',id ,@(mapcar #'car bindings)))
                                (,value ,exists-p (gethash ,args ,table)))
                          (values-list
                           (if ,exists-p ,value
                               (setf (gethash ,args ,table) (multiple-value-list (progn ,@body))))))))
                    ((list (lambda-list (and def (or 'cl:defun 'cl:defmethod)) name func-args &body (or (list* (and (list* 'cl:declare _) decl-p) body) body)
                                  &aux (declares (if decl-p (list decl-p))) (id (gensym "memo-"))))
                     (setf need-hashtablep t)
                     (assert (not (intersection '(&rest &allow-other-keys) func-args)) nil "can't memoize functions with &rest, &allow-other-keys in their defining lambda-lists")
                     `(,def ,name (,@func-args)
                        ,@declares
                        (letv* ((,args (list ',id ,@(mapcar #'(lambda (x) (first (ensure-list x))) (set-difference func-args cl:lambda-list-keywords))))
                                (,value ,exists-p (gethash ,args ,table)))
                          (values-list
                           (if ,exists-p ,value
                               (setf (gethash ,args ,table) (multiple-value-list (progn ,@body))))))))
                    ((list (lambda-list (and def (or 'cl:labels 'cl:flet)) definitions &body body))
                     (setf need-hashtablep t)
                     `(,def (,@(mapcar #'(lambda (x) (cdr (transformer `(memoizing (cl:defun ,@x))))) definitions))
                          ,@body))
                    ((lambda-list code &key (type nil type?) (bind (gensym)))
                     (if-let ((cv (rassoc code cache :key #'first :test #'equal)))
                       (first cv)
                       (values (list* bind code (if type? `(:type ,type)))
                               #'(lambda (f decl)
                                   (push (list* (first decl) (funcall f (second decl)) (cddr decl)) cache)
                                   (first decl))))
                     #+nil(error "don't know how to memoize ~a" code)))))))
      (let ((transformed-body (std/list:maptree '(memoizing with-memoization quote) #'transformer body)))
        `(let*-typed (,@(if need-hashtablep `((,table ,hash-table)))
                        ,@(reverse cache))
           ,@transformed-body)))))
