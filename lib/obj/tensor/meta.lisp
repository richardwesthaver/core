;;; meta.lisp --- Tensor MOP

;; 

;;; Commentary:

;; [[id:0521332c-11d2-4ffc-8ada-99690b8b2655][dispatch strategy for tensor methods]]

;; DEFTENSOR - define a tensor object class

;; DEFINE-TENSOR-METHOD - define a tensor method

;;; Code:
(in-package :obj/tensor)
(definline lazy-coerce (x output-type-spec)
  (if (typep x output-type-spec) x
      (let ((ret (copy x output-type-spec)))
        (when (slot-exists-p x 'memos) (maphash (lambda (k v) (setf (gethash k (memos ret)) v)) (memos x)))
        ret)))

(defun real-subtypep (type) 
  (and (listp type) (>= (length type) 1)
       (if (> (length type) 1)
           (second type)
           t)))

(defun cclass-max (&rest lst)
  (loop for ele in lst 
        with max
        do
           (when (or (null max)
                     (and (coerceable? max ele)
                          (or (not (coerceable? ele max))
                              (and (float-tensorp ele) (float-tensorp max)
                                   (> (float-digits (coerce 0 (or (real-subtypep (field-type ele)) (field-type ele))))
                                      (float-digits (coerce 0 (or (real-subtypep (field-type max)) (field-type max)))))))))
             (setf max ele))
        finally (return max)))

(defclass tensor-class (standard-class) 
  ((field-type :reader field-type)))

(defmethod sb-mop:validate-superclass ((class tensor-class) (superclass standard-class))  t)
(defmethod sb-mop:validate-superclass ((class tensor-class) (superclass kernel-class)) t)
(defmethod field-type ((class symbol)) (field-type (find-class class)))

(defmacro deftensor (name supers slots &rest options)
  `(defclass ,name ,supers ,slots ,@options (:metaclass tensor-class)))

(defclass tensor-method-generator (standard-generic-function) ()
  (:metaclass funcallable-standard-class))
(defmethod sb-mop:validate-superclass ((class tensor-method-generator) (superclass standard-generic-function)) t)

(defclass classp-specializer (specializer)
  ((object-class :initform nil :initarg :object-class)
   (direct-methods :initform nil :reader specializer-direct-methods))
  (:documentation "Exact class specializer."))
(defmethod print-object ((obj classp-specializer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ", ~a" (class-name (slot-value obj 'object-class)))))

(defmethod add-direct-method ((specializer classp-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod remove-direct-method ((specializer classp-specializer) method)
  (setf (slot-value specializer 'direct-methods)
        (remove method (slot-value specializer 'direct-methods))))
(defmethod make-load-form ((obj classp-specializer) &optional env)
  (declare (ignore env))
  (values `(classp-specializer ',(class-name (slot-value obj 'object-class))) nil))

(defclass group-specializer (specializer)
  ((object-class :initform nil :initarg :object-class)
   (group-name :initform nil :initarg :group-name)
   (direct-methods :initform nil :reader specializer-direct-methods))
  (:documentation "Applicable for each group-specializer with distinct
GROUP-NAME, the classes of the respective argument are the same."))
(defmethod print-object ((obj group-specializer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ", ~a, ~a" (class-name (slot-value obj 'object-class)) (slot-value obj 'group-name))))

(defmethod add-direct-method ((specializer group-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod remove-direct-method ((specializer group-specializer) method)
  (setf (slot-value specializer 'direct-methods)
        (remove method (slot-value specializer 'direct-methods))))
(defmethod make-load-form ((obj group-specializer) &optional env)
  (declare (ignore env))
  (values `(group-specializer ',(class-name (slot-value obj 'object-class)) ',(slot-value obj 'group-name)) nil))
;;Subtype
(defclass subtype-specializer (specializer)
  ((specializer-type :initform nil :initarg :specializer-type)
   (direct-methods :initform nil :reader specializer-direct-methods))
  (:documentation "Applicable only if for each group-specializer with distinct @argument{group-name}, the classes of the respective argument are the same."))
(defmethod print-object ((obj subtype-specializer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ", ~a" (slot-value obj 'specializer-type))))

;; (defmethod sb-pcl::specializer-type-specifier (proto-generic proto-method (specializer group-specializer))
;;   (declare (ignore proto-generic proto-method))
;;   (slot-value specializer 'specializer-type))

(defmethod add-direct-method ((specializer subtype-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod remove-direct-method ((specializer subtype-specializer) method)
  (setf (slot-value specializer 'direct-methods)
        (remove method (slot-value specializer 'direct-methods))))
(defmethod make-load-form ((obj subtype-specializer) &optional env)
  (declare (ignore env))
  (values `(subtype-specializer ',(slot-value obj 'specializer-type)) nil))

(defparameter *specializer-table* (make-hash-table :test 'equal))
(with-memoization (*specializer-table*)
  (memoizing
   (defun classp-specializer (class-name)
     (make-instance 'classp-specializer :object-class (find-class class-name))))
  (memoizing
   (defun group-specializer (class-name group-name)
     (make-instance 'group-specializer :object-class (find-class class-name) :group-name (the keyword group-name))))
  (memoizing
   (defun subtype-specializer (specializer-type)
     (make-instance 'subtype-specializer :specializer-type specializer-type))))

(defmethod compute-applicable-methods-using-classes ((gf tensor-method-generator) required-classes)
  (loop named mc
        for m in (generic-function-methods gf)
        with class-info-enoughp = t
        with applicable-methods
        do (loop for s in (method-specializers m)
                 for c in required-classes with group-keys = nil
                 do
                    (etypecase s
                      (class (subtypep c s))
                      (eql-specializer (and (eql c (class-of (eql-specializer-object s)))
                                            (not (setf class-info-enoughp nil))))
                      (subtype-specializer (and (or (eql c (find-class 'symbol)) (eql c (find-class 'list)))
                                                (not (setf class-info-enoughp nil))))
                      (group-specializer
                       (let ((key-name (slot-value s 'group-name)))
                         (if-let ((key (assoc key-name group-keys)))
                           (eql (cdr key) c)
                           (when (subtypep c (slot-value s 'object-class))
                             (push (cons key-name c) group-keys) t))))
                      (classp-specializer (eq c (slot-value s 'object-class))))
                 finally (push m applicable-methods))
        finally (return-from mc
                  (values 
                   (sort (copy-list applicable-methods) 
                         #'(lambda (m1 m2) (method-more-specific-p m1 m2 required-classes)))
                   class-info-enoughp))))

(defmethod compute-applicable-methods ((gf tensor-method-generator) arguments &aux (argument-classes (mapcar #'class-of arguments)))
  (loop named mc
        for m in (generic-function-methods gf)
        with applicable-methods = nil
        do (loop for s in (method-specializers m)
                 for a in arguments with group-keys = nil
                 do (etypecase s
                      (class (typep a s))
                      (eql-specializer (and (eql a (eql-specializer-object s))))
                      (subtype-specializer (subtypep a (slot-value s 'specializer-type)))
                      (group-specializer
                       (let ((key-name (slot-value s 'group-name)))
                         (if-let ((key (assoc key-name group-keys)))
                           (eql (cdr key) (class-of a))
                           (when (typep a (slot-value s 'object-class))
                             (push (cons key-name (class-of a)) group-keys) t))))
                      (classp-specializer (eq (class-of a) (slot-value s 'object-class))))
                 finally (push m applicable-methods))
        finally (return-from mc 
                  (sort (copy-list applicable-methods) 
                        #'(lambda (m1 m2) (method-more-specific-p m1 m2 argument-classes))))))

;;Borrowed from AMOP p.122
(defun method-more-specific-p (method1 method2 required-classes)
  (map nil #'(lambda (spec1 spec2 arg-class)
               (unless (or (eq spec1 spec2)
                           (std/list:cart-typecase (spec1 spec2)
                             ((classp-specializer classp-specializer) (eq (slot-value spec1 'object-class) (slot-value spec1 'object-class)))
                             ((group-specializer group-specializer) (and (eq (slot-value spec1 'object-class) (slot-value spec1 'object-class))
                                                                         (eq (slot-value spec1 'group-name) (slot-value spec1 'group-name))))
                             ((subtype-specializer subtype-specializer) (and (eq (slot-value spec1 'specializer-type) (slot-value spec2 'specializer-type))))))
                 (return-from method-more-specific-p (sub-specializer-p spec1 spec2 arg-class))))
       (method-specializers method1)
       (method-specializers method2)
       required-classes))

(defun sub-specializer-p (spec1 spec2 arg-class)
  (cart-typecase (spec1 spec2)
    ((class class) (not (null (find spec2 (cdr (member spec1 (class-precedence-list arg-class)))))))
    ((classp-specializer classp-specializer) (sub-specializer-p (the class (slot-value spec1 'object-class)) (the class (slot-value spec2 'object-class)) arg-class))
    ;;classp-specializer in list if spec1.object-class = arg-class
    ((classp-specializer class) t)
    ((group-specializer class)
     (if (or (eq (slot-value spec1 'object-class) spec2)
             (sub-specializer-p (slot-value spec1 'object-class) spec2 arg-class))
         t nil))
    ((class group-specializer) (sub-specializer-p spec1 (slot-value spec2 'object-class) arg-class))
    ((classp-specializer group-specializer)
     (or (eq (slot-value spec1 'object-class) (slot-value spec2 'object-class))
         (sub-specializer-p (slot-value spec1 'object-class) (slot-value spec2 'object-class) arg-class)))
    ((group-specializer classp-specializer)
     (sub-specializer-p (slot-value spec1 'object-class) (slot-value spec2 'object-class) arg-class))
    ((subtype-specializer (or group-specializer classp-specializer class)) t)
    ((subtype-specializer subtype-specializer) (subtypep (slot-value spec1 'specializer-type) (slot-value spec2 'specializer-type)))
    ((eql-specializer t) t)))

(defparameter *template-generated-methods* (make-hash-table :test 'equal))
;;(subclassp (find-class (tensor 'double-float)) (find-class 'base-tensor))
(defmacro define-tensor-method (name (&rest args) &body body)
  (let* ((keypos (or (position-if (lambda (x) (member x cl:lambda-list-keywords)) args) (length args)))
         (dispatch-args (subseq args 0 keypos))
         (dispatch-key (mapcar (lambda (x) (if (consp x) (second x) t)) dispatch-args))
         (generate-args (remove-if-not #'(lambda (x) (and (consp x) (cddr x))) dispatch-args))
         (generate-groups (loop for ele in generate-args with ret = nil 
                                do (setf ret (union ret (list (third ele))))
                                finally (return ret))))
    (with-gensyms (xx value existsp type-methods func)
      `(eval-every
         ;;clear methods
         (letv* ((,value ,existsp (gethash ',name *template-generated-methods*)))
           (if ,existsp
               (if-let ((,type-methods (assoc ',dispatch-key (cdr ,value) :test #'equal)))
                 (loop for ,func in (cdr ,type-methods)
                       do (remove-method (function ,name) ,func)
                       finally (setf (cdr ,type-methods) nil))
                 (setf (cdr ,value) (list* (list ',dispatch-key) (cdr ,value))))
               (setf (gethash ',name *template-generated-methods*) (list ',name (list ',dispatch-key)))))
         ;;generic type coercer.
         ,@(let* ((coerce-groups (remove-if-not #'(lambda (x) (< 1 (length (remove-if-not #'(lambda (y) (eql x (third y))) generate-args)))) generate-groups))
                  (sym (zipsym coerce-groups)))
             (when coerce-groups
               `((defmethod ,name (,@(mapcar #'(lambda (x) (if (consp x) (subseq x 0 2) x)) (subseq args 0 keypos)) ,@(subseq args keypos))
                   (let (,@(loop for (ts g) in sym
                                 collect `(,ts ,(std/curry:rec c+ ((lst (remove-if-not #'(lambda (x) (eql g (third x))) generate-args)))
                                                  (when lst `(cclass-max (type-of ,(first (car lst))) ,(c+ (cdr lst))))))))
                     ,@(loop for (ts g) in sym
                             append
                                (mapcar #'(lambda (x)
                                            `(assert (eql ,ts (type-of ,(first x))) nil "output type clash: don't know how to generate code for the given arguments."))
                                        (remove-if-not #'(lambda (x) (and (eql g (third x)) (fourth x))) generate-args)))
                     ,@(let ((dargs (mapcar #'(lambda (x)
                                                (cond
                                                  ((and (listp x) (>= (length x) 3))
                                                   (destructuring-bind (name _ group &optional destructive) x
                                                     (declare (ignore _))
                                                     (if (or destructive 
                                                             (not (member group coerce-groups)))
                                                         name 
                                                         `(lazy-coerce ,name ,(first (rassoc (list group) sym :test #'equal))))))
                                                  ((and (listp x) (= (length x) 2)) (car x))
                                                  (t x)))
                                            (subseq args 0 keypos))))
                         (if-let ((rest-pos (position '&rest args)))
                           `((apply #',name (list* ,@dargs ,@(mapcar #'(lambda (x) (first (ensure-list x))) (set-difference (subseq args keypos rest-pos) cl:lambda-list-keywords))
                                                   ,(elt args (1+ rest-pos)))))
                           `((,@(if (symbolp name) `(,name) `(funcall #',name)) ,@dargs ,@(mapcar #'(lambda (x) (first (ensure-list x))) (set-difference (subseq args keypos) cl:lambda-list-keywords)))))))))))
         ;;method generator.
         ,@(let ((sym (zipsym generate-groups)))
             `((defmethod ,name 
                   (,@(mapcar (lambda (x) 
                                (cond
                                  ((and (listp x) (>= (length x) 3))
                                   (destructuring-bind (name dispatch group &optional _) x
                                     (declare (ignore _))
                                     `(,name ,(group-specializer dispatch group))))
                                  (t x)))
                       (subseq args 0 keypos))
                    ,@(subseq args keypos))
                 (let (,@(loop for (tg g) in sym collect `(,tg (type-of ,(first (find-if #'(lambda (x) (eql (third x) g)) generate-args)))))
                       (,xx (or (assoc ',dispatch-key (cdr (gethash ',name *template-generated-methods*)) :test #'equal)
                                (error "Method table missing from *template-generated-methods*!"))))
                   (push
                    (macrolet ((cl (,xx) (ecase ,xx ,@(mapcar #'(lambda (x) `(,(second x) (quote ,(first x))))  sym))))
                      (compile-and-eval
                       `(defmethod ,',name (,@(list ,@(mapcar 
                                                       (lambda (x) 
                                                         (cond
                                                           ((and (listp x) (>= (length x) 3))
                                                            (destructuring-bind (name _ group &optional _1) x
                                                              (declare (ignore _ _1))
                                                              `(list ',name (classp-specializer (cl ,group)))))
                                                           (t `(quote ,x))))
                                                       (subseq args 0 keypos)))
                                            ,@',(subseq args keypos))
                          ,@(list ,@body))))
                    (cdr ,xx)))
                 ,@(let ((dargs (mapcar #'(lambda (x) (first (ensure-list x))) (subseq args 0 keypos))))
                     (if-let ((rest-pos (position '&rest args)))
                       `((apply #',name (list* ,@dargs ,@(mapcar #'(lambda (x) (first (ensure-list x))) (set-difference (subseq args keypos rest-pos) lambda-list-keywords))
                                               ,(elt args (1+ rest-pos)))))
                       `((,@(if (symbolp name) `(,name) `(funcall #',name)) ,@dargs ,@(mapcar #'(lambda (x) (first (ensure-list x))) (remove-if #'(lambda (x) (member x lambda-list-keywords)) (subseq args keypos))))))))))))))
