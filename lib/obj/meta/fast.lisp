;;; obj/meta/fast.lisp --- Fast generic functions

;; see https://github.com/marcoheisig/fast-generic-functions

;;; Code:
(in-package :obj/meta/fast)

(deftype local-variable ()
  '(and symbol (not (satisfies constantp))))

(defclass required-info ()
  ((%variable
    :initarg :variable
    :reader required-info-variable
    :type local-variable
    :initform (required-argument :variable))))

(defclass optional-info ()
  ((%variable
    :initarg :variable
    :reader optional-info-variable
    :type local-variable
    :initform (required-argument :variable))
   (%initform
    :initarg :initform
    :reader optional-info-initform
    :initform nil)
   (%suppliedp
    :initarg :suppliedp
    :reader optional-info-suppliedp
    :type (or null local-variable)
    :initform nil)))

(defclass keyword-info ()
  ((%keyword
    :initarg :keyword
    :reader keyword-info-keyword
    :type keyword
    :initform (required-argument :keyword))
   (%variable
    :initarg :variable
    :reader keyword-info-variable
    :type local-variable
    :initform (required-argument :variable))
   (%initform
    :initarg :initform
    :reader keyword-info-initform
    :initform nil)
   (%suppliedp
    :initarg :suppliedp
    :reader keyword-info-suppliedp
    :type (or null local-variable)
    :initform nil)))

(defclass auxiliary-info ()
  ((%variable
    :initarg :variable
    :reader auxiliary-info-variable
    :type local-variable
    :initform (required-argument :variable))
   (%initform
    :initarg :initform
    :reader auxiliary-info-initform
    :initform nil)))

(defun parse-ordinary-lambda-list (lambda-list)
  "Returns six values:

 1. A list of REQUIRED-INFO instances, one for each required argument.

 2. A list of OPTIONAL-INFO instances, one for each optional argument.

 3. The name of the rest variable, or NIL, if there is none.

 4. A list of KEYWORD-INFO instances, one for each keyword argument.

 5. A boolean, indicating whether &allow-other-keys is present.

 6. A list of AUXILIARY-INFO instances, one for each auxiliary argument.

Can parse all but specialized lambda lists.
"
  (let ((required '())
        (optional '())
        (keyword '())
        (auxiliary '())
        (rest-var nil)
        (allow-other-keys-p nil))
    (labels ((fail ()
               (error "Malformed lambda list: ~S" lambda-list))
             (parse-required (lambda-list)
               (unless (endp lambda-list)
                 (let ((item (first lambda-list)))
                   (case item
                     (&optional (parse-&optional (rest lambda-list)))
                     (&rest (parse-&rest (rest lambda-list)))
                     (&key (parse-&key (rest lambda-list)))
                     (&aux (parse-&aux (rest lambda-list)))
                     (#.(set-difference lambda-list-keywords '(&optional &rest &key &aux))
                      (fail))
                     (otherwise
                      (push (parse-reqired-item item) required)
                      (parse-required (rest lambda-list)))))))
             (parse-&optional (lambda-list)
               (unless (endp lambda-list)
                 (let ((item (first lambda-list)))
                   (case item
                     (&rest (parse-&rest (rest lambda-list)))
                     (&key (parse-&key (rest lambda-list)))
                     (&aux (parse-&aux (rest lambda-list)))
                     (#.(set-difference lambda-list-keywords '(&rest &key &aux))
                      (fail))
                     (otherwise
                      (push (parse-optional-item item) optional)
                      (parse-&optional (rest lambda-list)))))))
             (parse-&rest (lambda-list)
               (unless (consp lambda-list)
                 (fail))
               (let ((item (first lambda-list)))
                 (unless (symbolp item)
                   (fail))
                 (unless (null rest-var)
                   (fail))
                 (setf rest-var item)
                 (unless (endp (rest lambda-list))
                   (case (first (rest lambda-list))
                     (&key (parse-&key (rest (rest lambda-list))))
                     (&aux (parse-&aux (rest (rest lambda-list))))
                     (otherwise (fail))))))
             (parse-&key (lambda-list)
               (unless (endp lambda-list)
                 (let ((item (first lambda-list)))
                   (case item
                     (&allow-other-keys (parse-&allow-other-keys (rest lambda-list)))
                     (&aux (parse-&aux (rest lambda-list)))
                     (#.(set-difference lambda-list-keywords '(&allow-other-keys &aux))
                      (fail))
                     (otherwise
                      (push (parse-keyword-item item) keyword)
                      (parse-&key (rest lambda-list)))))))
             (parse-&allow-other-keys (lambda-list)
               (setf allow-other-keys-p t)
               (unless (endp lambda-list)
                 (case (first lambda-list)
                   (&aux (parse-&aux (rest lambda-list)))
                   (otherwise
                    (fail)))))
             (parse-&aux (lambda-list)
               (unless (endp lambda-list)
                 (let ((item (first lambda-list)))
                   (case item
                     (#.lambda-list-keywords (fail))
                     (otherwise
                      (push (parse-auxiliary-item item) auxiliary)
                      (parse-&aux (rest lambda-list))))))))
      (parse-required lambda-list))
    (values
     (nreverse required)
     (nreverse optional)
     rest-var
     (nreverse keyword)
     allow-other-keys-p
     (nreverse auxiliary))))

(defun parse-reqired-item (item)
  (unless (typep item 'local-variable)
    (error "Not a valid lambda list variable: ~S"
           item))
  (make-instance 'required-info
    :variable item))

(defun parse-optional-item (item)
  (typecase item
    (local-variable
     (make-instance 'optional-info
       :variable item))
    ((cons local-variable null)
     (make-instance 'optional-info
       :variable (first item)))
    ((cons local-variable (cons t null))
     (make-instance 'optional-info
       :variable (first item)
       :initform (second item)))
    ((cons local-variable (cons t (cons local-variable null)))
     (make-instance 'optional-info
       :variable (first item)
       :initform (second item)
       :suppliedp (third item)))
    (t (error "Invalid &optional lambda list item: ~S"
              item))))

(defun parse-keyword-item (item)
  (labels ((fail ()
             (error "Invalid &key lambda list item: ~S"
                    item))
           (parse-keyword-var (item)
           (etypecase item
             (symbol
              (values (intern (symbol-name item) :keyword)
                      item))
             ((cons symbol null)
              (values (intern (symbol-name (first item)) :keyword)
                      (first item)))
             ((cons keyword (cons symbol null))
              (values (first item)
                      (second item)))
             (t (fail)))))
    (typecase item
      (local-variable
       (make-instance 'keyword-info
         :variable item
         :keyword (intern (symbol-name item) :keyword)))
      ((cons t null)
       (multiple-value-bind (keyword variable)
           (parse-keyword-var (first item))
         (make-instance 'keyword-info
           :variable variable
           :keyword keyword)))
      ((cons t (cons t null))
       (multiple-value-bind (keyword variable)
           (parse-keyword-var (first item))
         (make-instance 'keyword-info
           :variable variable
           :keyword keyword
           :initform (second item))))
      ((cons t (cons t (cons local-variable null)))
       (multiple-value-bind (keyword variable)
           (parse-keyword-var (first item))
         (make-instance 'keyword-info
           :variable variable
           :keyword keyword
           :initform (second item)
           :suppliedp (third item))))
      (t (fail)))))

(defun parse-auxiliary-item (item)
  (typecase item
    (local-variable
     (make-instance 'auxiliary-info
       :variable item))
    ((cons local-variable null)
     (make-instance 'auxiliary-info
       :variable (first item)))
    ((cons local-variable (cons t null))
     (make-instance 'auxiliary-info
       :variable (first item)
       :initform (second item)))
    (t (error "Invalid &aux lambda list item: ~S"
              item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda List Unparsing

(defun unparse-ordinary-lambda-list
    (required optional rest-var keyword allow-other-keys-p auxiliary)
  (append
   (unparse-required required)
   (unparse-optional optional)
   (unparse-rest rest-var)
   (unparse-keyword keyword allow-other-keys-p)
   (unparse-auxiliary auxiliary)))

(defun unparse-required (required)
  (mapcar
   (lambda (info)
     (required-info-variable info))
   required))

(defun unparse-optional (optional)
  (if (null optional)
      `()
      `(&optional
        ,@(mapcar
           (lambda (info)
             `(,(optional-info-variable info)
               ,(optional-info-initform info)
               ,@(if (optional-info-suppliedp info)
                     `(,(optional-info-suppliedp info))
                     `())))
           optional))))

(defun unparse-keyword (keyword allow-other-keys-p)
  (if (and (null keyword)
           (not allow-other-keys-p))
      `()
      `(&key
        ,@(mapcar
           (lambda (info)
             `((,(keyword-info-keyword info) ,(keyword-info-variable info))
               ,(keyword-info-initform info)
               ,@(if (keyword-info-suppliedp info)
                     `(,(keyword-info-suppliedp info))
                     `())))
           keyword)
        ,@(if allow-other-keys-p
              '(&allow-other-keys)
              '()))))

(defun unparse-rest (rest-var)
  (if (null rest-var)
      `()
      `(&rest ,rest-var)))

(defun unparse-auxiliary (auxiliary)
  (if (null auxiliary)
      `()
      `(&aux
        ,@(mapcar
           (lambda (info)
             (list (auxiliary-info-variable info)
                   (auxiliary-info-initform info)))
           auxiliary))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda List Info Anonymization

(defun anonymize-ordinary-lambda-list (lambda-list)
  (multiple-value-bind (required optional rest-var keyword allow-other-keys-p auxiliary)
      (parse-ordinary-lambda-list lambda-list)
    (unparse-ordinary-lambda-list
     (mapcar #'anonymize-required-info required)
     (mapcar #'anonymize-optional-info optional)
     (if (null rest-var)
         nil
         (gensymify rest-var))
     (mapcar #'anonymize-keyword-info keyword)
     allow-other-keys-p
     (mapcar #'anonymize-auxiliary-info auxiliary))))

(defun anonymize-required-info (info)
  (make-instance 'required-info
    :variable (gensymify (required-info-variable info))))

(defun anonymize-optional-info (info)
  (make-instance 'optional-info
    :variable (gensymify (optional-info-variable info))
    :initform (optional-info-initform info)
    :suppliedp (if (optional-info-suppliedp info)
                   (gensymify (optional-info-suppliedp info))
                   nil)))

(defun anonymize-keyword-info (info)
  (make-instance 'keyword-info
    :variable (gensymify (keyword-info-variable info))
    :keyword (keyword-info-keyword info)
    :initform (keyword-info-initform info)
    :suppliedp (if (keyword-info-suppliedp info)
                   (gensymify (keyword-info-suppliedp info))
                   nil)))

(defun anonymize-auxiliary-info (info)
  (make-instance 'auxiliary-info
    :variable (gensymify (auxiliary-info-variable info))
    :initform (auxiliary-info-initform info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous

(defun normalize-ordinary-lambda-list (lambda-list)
  (multiple-value-call #'unparse-ordinary-lambda-list
    (parse-ordinary-lambda-list lambda-list)))

(defun lambda-list-variables (lambda-list)
  (multiple-value-bind (required optional rest-var keyword allow-other-keys-p auxiliary)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other-keys-p))
    (let ((variables '()))
      (dolist (info required)
        (push (required-info-variable info) variables))
      (dolist (info optional)
        (push (optional-info-variable info) variables)
        (when (optional-info-suppliedp info)
          (push (optional-info-suppliedp info) variables)))
      (unless (null rest-var)
        (push rest-var variables))
      (dolist (info keyword)
        (push (keyword-info-variable info) variables)
        (when (keyword-info-suppliedp info)
          (push (keyword-info-suppliedp info) variables)))
      (dolist (info auxiliary)
        (push (auxiliary-info-variable info) variables))
      (nreverse variables))))

(defun lambda-list-apply-arguments (lambda-list)
  (multiple-value-bind (required optional rest-var keyword)
      (parse-ordinary-lambda-list lambda-list)
    (append
     (mapcar #'required-info-variable required)
     (mapcar #'optional-info-variable optional)
     (if rest-var
         `(,rest-var)
         `(,@(loop for info in keyword
                   collect (keyword-info-keyword info)
                   collect (keyword-info-variable info))
           '())))))

;;; expand-effective-method-body
(defun expand-effective-method-body
    (effective-method generic-function lambda-list)
  (macroexpand-all
   `(let ((.gf. #',(sb-mop:generic-function-name generic-function)))
      (declare (ignorable .gf.))
      (declare (sb-ext:disable-package-locks common-lisp:call-method))
      (declare (sb-ext:disable-package-locks common-lisp:make-method))
      (declare (sb-ext:disable-package-locks sb-pcl::check-applicable-keywords))
      (declare (sb-ext:disable-package-locks sb-pcl::no-primary-method))
      (macrolet
          (;; SBCL introduces explicit keyword argument checking into
           ;; the effective method.  Since we do our own checking, we
           ;; can safely disable it.  However, we touch the relevant
           ;; variables to prevent unused variable warnings.
           #+sbcl
           (sb-pcl::check-applicable-keywords (&rest args)
             (declare (ignore args))
             `(progn sb-pcl::.valid-keys. sb-pcl::.keyargs-start. (values)))
           ;; SBCL introduces a magic form to report when there are no
           ;; primary methods.  The problem is that this form contains a
           ;; reference to the literal generic function, which is not an
           ;; externalizable object.  Our solution is to replace it with
           ;; something portable.
           #+sbcl
           (sb-pcl::no-primary-method (&rest args)
             (declare (ignore args))
             `(apply #'no-primary-method .gf. ,@',(lambda-list-apply-arguments lambda-list))))
        ,(wrap-in-call-method-macrolet
          effective-method
          generic-function
          lambda-list)))))

(defun wrap-in-call-method-macrolet (form generic-function lambda-list)
  `(macrolet ((call-method (method &optional next-methods)
                (expand-call-method
                 method
                 next-methods
                 ',lambda-list
                 ',(class-name
                    (sb-mop:generic-function-method-class generic-function)))))
     ,(wrap-in-reinitialize-arguments form lambda-list)))

(defun wrap-in-reinitialize-arguments (form lambda-list)
  (let ((anonymized-lambda-list
          (anonymize-ordinary-lambda-list lambda-list)))
    `(flet ((reinitialize-arguments ,anonymized-lambda-list
              ,@(mapcar
                 (lambda (place value)
                   `(setf ,place ,value))
                 (lambda-list-variables lambda-list)
                 (lambda-list-variables anonymized-lambda-list))))
       (declare (ignorable #'reinitialize-arguments))
       (declare (inline reinitialize-arguments))
       ,form)))

(defun expand-call-method (method next-methods lambda-list method-class)
  (wrap-in-next-methods
   (call-fast-method-lambda
    (coerce-to-fast-method method lambda-list method-class)
    lambda-list)
   next-methods
   lambda-list
   method-class))

(defun coerce-to-fast-method (method lambda-list method-class)
  (cond ((typep method 'fast-method)
         method)
        ((and (consp method)
              (eql (car method) 'make-method)
              (null (cddr method)))
         (make-instance method-class
           :lambda-list lambda-list
           :specializers (make-list (length (parse-ordinary-lambda-list lambda-list))
                                    :initial-element (find-class 't))
           :qualifiers '()
           :function #'values
           'lambda
           `(lambda ,lambda-list
              (declare (ignorable ,@(lambda-list-variables lambda-list)))
              ,(second method))))
        (t
         (error "Cannot turn ~S into an inlineable method."
                method))))

(defun wrap-in-next-methods (form next-methods lambda-list method-class)
  (if (null next-methods)
      `(flet ((next-method-p () nil)
              (call-next-method ()
                (apply
                 #'no-next-method
                 .gf.
                 (class-prototype (find-class ',method-class))
                 ,@(lambda-list-apply-arguments lambda-list))))
         (declare (ignorable #'next-method-p #'call-next-method))
         ,form)
      (wrap-in-next-methods
       `(flet ((next-method-p () t)
               (call-next-method (&rest args)
                 (unless (null args)
                   (apply #'reinitialize-arguments args))
                 (call-method ,(first next-methods) ,(rest next-methods))))
          (declare (ignorable #'next-method-p #'call-next-method))
          ,form)
       (rest next-methods)
       lambda-list
       method-class)))

(defun call-fast-method-lambda (method lambda-list)
  (multiple-value-bind (g-required g-optional g-rest-var g-keyword)
      (parse-ordinary-lambda-list lambda-list)
    (multiple-value-bind (m-required m-optional m-rest-var m-keyword)
        (parse-ordinary-lambda-list (sb-mop:method-lambda-list method))
      ;; Assert that the method has arguments that are congruent to those
      ;; of the corresponding generic function.
      (assert (or (= (length g-required)
                     (length m-required))))
      (assert (= (length g-optional)
                 (length m-optional)))
      (when (null g-rest-var)
        (assert (null m-rest-var)))
      `(funcall
        ,(fast-method-lambda method)
        ;; Required arguments.
        ,@(mapcar #'required-info-variable g-required)
        ;; Optional arguments.
        ,@(loop for g-info in g-optional
                for m-info in m-optional
                append
                (if (null (optional-info-suppliedp g-info))
                    `(,(optional-info-variable g-info))
                    (let ((value
                            `(if ,(optional-info-suppliedp g-info)
                                 ,(optional-info-variable g-info)
                                 ,(optional-info-initform m-info))))
                      (if (null (optional-info-suppliedp m-info))
                          `(,value)
                          `(,value ,(optional-info-suppliedp g-info))))))
        ;; The rest argument.
        ,@(if (null m-rest-var)
              `()
              `(,g-rest-var))
        ;; Keyword arguments.
        ,@(loop for m-info in m-keyword
                for g-info = (find (keyword-info-keyword m-info) g-keyword
                                   :key #'keyword-info-keyword)
                append
                (if (null (keyword-info-suppliedp g-info))
                    `(,(keyword-info-variable g-info))
                    (let ((value
                            `(if ,(keyword-info-suppliedp g-info)
                                 ,(keyword-info-variable g-info)
                                 ,(keyword-info-initform m-info))))
                      (if (null (keyword-info-suppliedp m-info))
                          `(,value)
                          `(,value ,(keyword-info-suppliedp g-info))))))))))

;;; generic functions
(defgeneric optimize-function-call (generic-function static-call-signature))

;; may need to change this to conform with sb-pcl..
(defgeneric no-primary-method (generic-function &rest arguments)
  (:method ((generic-function generic-function) &rest arguments)
    (error "~@<No primary method for call to the generic function ~S with ~
             arguments ~S.~:@>"
           generic-function arguments)))

;;; fast-method
(defclass fast-method (potentially-sealable-standard-method)
  ((%lambda
    :initarg .lambda.
    :reader fast-method-lambda
    :initform (required-argument '.lambda.))))

(defmethod validate-method-property
    ((method fast-method) (property (eql 'inlineable)))
  t)

(defmethod make-method-lambda :around
    ((gf sealable-standard-generic-function)
     (fast-method fast-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list*
      '.lambda.
      (make-fast-method-lambda gf fast-method lambda environment)
      initargs))))

;; utility for the function below
(defun block-name (function-name)
  (etypecase function-name
    ((and symbol (not null)) function-name)
    ((cons (eql setf) (cons symbol null)) (second function-name))))

(defun make-fast-method-lambda
    (generic-function method lambda environment)
  (declare (ignore method))
  (destructuring-bind (lambda-symbol lambda-list &rest body) lambda
    (assert (eql lambda-symbol 'lambda))
    (multiple-value-bind (required optional rest-var keyword allow-other-keys-p auxiliary)
        (parse-ordinary-lambda-list lambda-list)
      (multiple-value-bind (forms declarations)
          (parse-body body)
        (let ((partially-flattened-lambda-list
                `(,@(lambda-list-variables
                     (unparse-ordinary-lambda-list
                      required optional rest-var keyword allow-other-keys-p '()))
                  ,@(unparse-ordinary-lambda-list '() '() nil '() nil auxiliary))))
          (macroexpand-all
           `(lambda ,partially-flattened-lambda-list
              (declare (ignorable ,@(mapcar #'required-info-variable required)))
              ,@declarations
              (block ,(block-name (sb-mop:generic-function-name generic-function))
                ,@forms))
           environment))))))

(defclass fast-generic-function (sealable-standard-generic-function)
  ((%full-effective-method-cache :initform '() :accessor full-effective-method-cache)
   (%flat-effective-method-cache :initform '() :accessor flat-effective-method-cache))
  (:default-initargs
   :method-class (find-class 'fast-method))
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod compute-effective-method-function
    ((fgf fast-generic-function) effective-method options)
  (let ((lambda-list
          (anonymize-ordinary-lambda-list
           ;; Unfortunately, we don't know the list of applicable methods
           ;; anymore at this stage.  So instead, we consider all methods
           ;; applicable.
           (compute-effective-method-lambda-list fgf (sb-mop:generic-function-methods fgf)))))
    (compile
     nil
     `(lambda ,lambda-list
        ,(expand-effective-method-body effective-method fgf lambda-list)))))

;;; optimize-function-call
(defmethod optimize-function-call :around
    ((fast-generic-function fast-generic-function)
     (static-call-signature static-call-signature))
  (call-next-method))

(defmethod optimize-function-call
    ((fast-generic-function fast-generic-function)
     (static-call-signature static-call-signature))
  (let ((applicable-methods
          (compute-applicable-methods
           fast-generic-function
           (static-call-signature-prototypes static-call-signature))))
    (cond (;; Inline the entire effective method.
           (every #'inlineable-method-p applicable-methods)
           (effective-method-lambda fast-generic-function static-call-signature nil))
          ;; Inline only the optional/keyword parsing step.
          ((and (externalizable-object-p static-call-signature)
                (intersection (sb-mop:generic-function-lambda-list fast-generic-function)
                              '(&optional &key &rest)))
           (let ((lambda-list
                   (anonymize-ordinary-lambda-list
                    (compute-effective-method-lambda-list
                     fast-generic-function applicable-methods))))
             `(lambda ,lambda-list
                (funcall
                 (load-time-value
                  (the function
                       (lookup-flat-effective-method
                        #',(sb-mop:generic-function-name fast-generic-function)
                        ',static-call-signature)))
                 ,@(lambda-list-variables lambda-list)))))
          ;; Eliminate the dispatch function.
          ((externalizable-object-p static-call-signature)
           `(lambda (&rest args)
              (apply
               (load-time-value
                (the function
                     (lookup-full-effective-method
                      #',(sb-mop:generic-function-name fast-generic-function)
                      ',static-call-signature)))
               args)))
          ;; Give up.
          (t nil))))

(defun inlineable-method-p (method)
  (member 'inlineable (method-properties method)))

(defun effective-method-lambda
    (generic-function static-call-signature flatten-arguments)
  (let* ((applicable-methods
           (compute-applicable-methods
            generic-function
            (static-call-signature-prototypes static-call-signature)))
         (effective-method-lambda-list
           (compute-effective-method-lambda-list
            generic-function applicable-methods))
         (anonymized-lambda-list
           (anonymize-ordinary-lambda-list effective-method-lambda-list)))
    `(lambda ,(if flatten-arguments
                  (lambda-list-variables anonymized-lambda-list)
                  anonymized-lambda-list)
       (declare (optimize (safety 0)))
       ,@(loop for type in (static-call-signature-types static-call-signature)
               for argument in anonymized-lambda-list
               collect `(declare (ignorable ,argument))
               collect `(declare (type ,type ,argument)))
       (locally (declare (optimize (safety 1)))
         ,(expand-effective-method-body
           (sb-mop:compute-effective-method
            generic-function
            (sb-mop:generic-function-method-combination generic-function)
            applicable-methods)
           generic-function
           anonymized-lambda-list)))))

;;; Computing the Effective Method Lambda List

(defun merge-required-infos (g-required m-requireds)
  (dolist (m-required m-requireds g-required)
    (assert (= (length m-required)
               (length g-required)))))

(defun merge-optional-infos (g-optional m-optionals)
  (let ((n (length g-optional)))
    (dolist (m-optional m-optionals)
      (assert (= (length m-optional) n)))
    (unless (zerop n)
      (loop for g-info in g-optional
            for m-infos in (apply #'mapcar #'list m-optionals)
            collect
            ;; Now we have two cases - the one is that at least one method
            ;; cares about the suppliedp flag, the other one is that no
            ;; method cares.  Even if a method doesn't reference the
            ;; suppliedp flag itself, it may still need it to decide whether
            ;; to supply its initform or not.  Because of this, the suppliedp
            ;; parameter can only be discarded globally when the initforms of
            ;; all methods are constant and equal.
            (let ((global-initform (optional-info-initform (first m-infos)))
                  (no-one-cares (not (optional-info-suppliedp (first m-infos)))))
              (dolist (m-info m-infos)
                (with-accessors ((variable optional-info-variable)
                                 (initform optional-info-initform)
                                 (suppliedp optional-info-suppliedp))
                    m-info
                  (unless (and (constantp initform)
                               (equal initform global-initform)
                               (not suppliedp))
                    (setf no-one-cares nil))))
              (if no-one-cares
                  (make-instance 'optional-info
                    :variable (optional-info-variable g-info)
                    :initform global-initform)
                  (make-instance 'optional-info
                    :variable (optional-info-variable g-info)
                    :initform nil
                    :suppliedp (optional-info-suppliedp g-info))))))))

(defun merge-keyword-infos (g-keyword m-keywords)
  ;; First we assemble an alist whose keys are keywords and whose values
  ;; are all method keyword info objects that read this keyword.
  (let ((alist '()))
    (dolist (g-info g-keyword)
      (pushnew (list (keyword-info-keyword g-info)) alist))
    (dolist (m-keyword m-keywords)
      (dolist (m-info m-keyword)
        (let* ((key (keyword-info-keyword m-info))
               (entry (assoc key alist)))
          (if (consp entry)
              (push m-info (cdr entry))
              (push (list key m-info) alist)))))
    (loop for (key . m-infos) in alist
          collect
          ;; Merging keyword info objects is handled just like in the case
          ;; of optional info objects above.
          (let ((global-initform (keyword-info-initform (first m-infos)))
                (no-one-cares (not (keyword-info-suppliedp (first m-infos))))
                ;; Not actually g-info, but we need some place to grab a
                ;; variable name form.
                (g-info (or (find key g-keyword :key #'keyword-info-keyword)
                            (first m-infos))))
            (dolist (m-info m-infos)
              (with-accessors ((initform keyword-info-initform)
                               (suppliedp keyword-info-suppliedp))
                  m-info
                (unless (and (constantp initform)
                             (equal initform global-initform)
                             (not suppliedp))
                  (setf no-one-cares nil))))
            (if no-one-cares
                (make-instance 'keyword-info
                  :keyword key
                  :variable (keyword-info-variable g-info)
                  :initform global-initform)
                (make-instance 'keyword-info
                  :keyword key
                  :variable (keyword-info-variable g-info)
                  :initform nil
                  :suppliedp (or (keyword-info-suppliedp g-info)
                                 (gensymify "SUPPLIEDP"))))))))

(defun merge-allow-other-keys (g-allow-other-keys m-allow-other-keys-list)
  (reduce
   (lambda (a b) (or a b))
   m-allow-other-keys-list
   :initial-value g-allow-other-keys))

(defun compute-effective-method-lambda-list (generic-function applicable-methods)
  (multiple-value-bind (required optional rest-var keyword allow-other-keys)
      (parse-ordinary-lambda-list (sb-mop:generic-function-lambda-list generic-function))
    (let ((method-parses
            (mapcar
             (lambda (method)
               (multiple-value-list
                (parse-ordinary-lambda-list
                 (sb-mop:method-lambda-list method))))
             applicable-methods)))
      (unparse-ordinary-lambda-list
       (merge-required-infos required (mapcar #'first method-parses))
       (merge-optional-infos optional (mapcar #'second method-parses))
       rest-var
       (merge-keyword-infos keyword (mapcar #'fourth method-parses))
       (merge-allow-other-keys allow-other-keys (mapcar #'fifth method-parses))
       '()))))

;;; Effective Method Lookup
(declaim (ftype (function (t t) function) lookup-full-effective-method))
(declaim (ftype (function (t t) function) lookup-flat-effective-method))

(defun lookup-full-effective-method
    (generic-function static-call-signature)
  (with-accessors ((alist full-effective-method-cache)) generic-function
    (let* ((key (static-call-signature-types static-call-signature))
           (entry (assoc key alist :test #'equal)))
      (if (consp entry)
          (cdr entry)
          (let ((fn (compile nil (effective-method-lambda
                                  generic-function
                                  static-call-signature
                                  nil))))
            (push (cons key fn) alist)
            fn)))))

(defun lookup-flat-effective-method
    (generic-function static-call-signature)
  (with-accessors ((alist flat-effective-method-cache)) generic-function
    (let* ((key (static-call-signature-types static-call-signature))
           (entry (assoc key alist :test #'equal)))
      (if (consp entry)
          (cdr entry)
          (let ((fn (compile nil (effective-method-lambda
                                  generic-function
                                  static-call-signature
                                  t))))
            (push (cons key fn) alist)
            fn)))))

(defmethod seal-domain :after
    ((fast-generic-function fast-generic-function)
     (domain domain))
  (let ((name (sb-mop:generic-function-name fast-generic-function)))
    ;; Ensure that the function is known.
    (unless (sb-c::info :function :info name)
      (compile nil (eval `(sb-c:defknown ,name * * ()))))
    ;; Create an IR1-transform for each static call signature.
    (dolist (static-call-signature (compute-static-call-signatures fast-generic-function domain))
      (with-accessors ((types static-call-signature-types)
                       (prototypes static-call-signature-prototypes))
          static-call-signature
        (eval
         `(sb-c:deftransform ,name ((&rest args) (,@types &rest *))
            (or (optimize-function-call #',name ',static-call-signature)
                (sb-c::give-up-ir1-transform))))))))
