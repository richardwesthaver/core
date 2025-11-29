;;; memo.lisp --- Simple Memoization

;; 

;;; Code:
(in-package :std/macs)

;; copied from MATLISP, but with MATCH/EMATCH from the TRIVIA library factored
;; out.
;; (macroexpand (with-memoization (*ht*) (memoizing (defun foo (a b) (+ a b)))))
;; (foo 2 3) ; => ((#:MEMOIZE* 2 3) 5)
(defmacro with-memoization ((&optional (hash-table `(make-hash-table :test 'equal))) &body body &aux cache need-hashtablep)
  "Evaluate BODY with each form beginning with MEMOIZING having its result of
evaluation cached in HASH-TABLE."
  (with-gensyms (table value exists-p args)
    (labels ((transformer (x)
               (cond
                 ((and (listp x) (or (eql (car x) 'with-memoization) (eql (car x) 'quote))) x)
                 ((and (listp x) (eql (car x) 'memoizing)) ;; memoizing path
                  (let ((body (cadr x)))
                    (cond
                      ((and (listp body) (eql (car body) 'let))
                       ;; (list (lambda-list 'cl:let bindings &body (or (list* (and (list* 'cl:declare _) decl-p) body) body)
                       ;; &aux (declares (if decl-p (list decl-p))) (id (gensym "memo-"))))
                       (let ((bindings (cadr body))
                             (id (gensym "MEMO")))
                         (multiple-value-bind (body declares) (parse-body (cddr body))
                           (setf need-hashtablep t)
                           `(let (,@bindings)
                              ,@declares
                              (letv* ((,args (list ',id ,@(mapcar #'car bindings)))
                                      (,value ,exists-p (gethash ,args ,table)))
                                (values-list
                                 (if ,exists-p 
                                     ,value
                                     (setf (gethash ,args ,table) (multiple-value-list (progn ,@body))))))))))
                      ((and (listp body) (or (eql (car body) 'defun) (eql (car body) 'defmethod)))
                       (let ((def (car body))
                             (name (cadr body))
                             (fargs (caddr body))
                             (id (gensym "MEMO")))
                         (multiple-value-bind (body declares) (parse-body (cdddr body))
                           (setf need-hashtablep t)
                           (assert (not (intersection '(&rest &allow-other-keys) fargs)) nil "can't memoize functions with &rest, &allow-other-keys in their defining lambda-lists")
                           `(,def ,name (,@fargs)
                                ,@declares
                              (letv* ((,args (list ',id ,@(mapcar #'(lambda (x) (first (std/list:ensure-list x))) (set-difference fargs lambda-list-keywords))))
                                      (,value ,exists-p (gethash ,args ,table)))
                                (values-list
                                 (if ,exists-p ,value
                                     (setf (gethash ,args ,table) (multiple-value-list (progn ,@body))))))))))
                      ((and (listp body) (or (eql (car body) 'labels) (eql (car body) 'flet)))
                       (let ((def (car body))
                             (definitions (cadr body))
                             (body (cddr body)))
                         (setf need-hashtablep t)
                         `(,def (,@(mapcar #'(lambda (x) (cdr (transformer `(memoizing (defun ,@x))))) definitions))
                              ,@body)))
                      ((listp body)
                       (let ((code (car body))
                             (type (getf (cdr body) :type))
                             (bind (or (getf (cdr body) :bind) (gensym))))
                         (if-let ((cv (rassoc code cache :key #'first :test #'equal)))
                           (first cv)
                           (values (list* bind code (if type `(:type ,type)))
                                   #'(lambda (f decl)
                                       (push (list* (first decl) (funcall f (second decl)) (cddr decl)) cache)
                                       (first decl))))))))))))
      (let ((transformed-body (std/list:maptree '(memoizing with-memoization quote) #'transformer body)))
        `(lety* (,@(if need-hashtablep `((,table ,hash-table)))
                 ,@(reverse cache))
           ,@transformed-body)))))
