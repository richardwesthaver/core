;;; std/meta.lisp --- Standard MOP Utilities

;;

;;; Code:
(in-package :std/meta)

;;; Defverb
(sb-ext:defglobal *verbs* nil)

(defun register-verb (v) 
  ;; (let ((name (generic-function-name v)))
  ;;   (when (atom name)
  ;;     (setf (get (generic-function-name v) 'verb) t)))
  (pushnew v *verbs* :test #'equal :key #'generic-function-name))

(defmacro defverb (name args &rest props)
  "Like DEFGENERIC but specifically designed for verbs. The resulting function
object is pushed to *VERBS* and PROPS may contain the following additional
properties:

:ACCESSOR - when non-nil automatically define and register (setf NAME) as a
generic function too. Any :METHOD properties will only apply to the generic
function NAME and be skipped for (setf NAME)."
  (let* ((a (find :accessor props :key #'car)))
    (setf props (remove a props))
    `(progn
       (defgeneric ,name ,args ,@props)
       (register-verb (function ,name))
     ,@(when a
         `((defgeneric (setf ,name) ,(cons 'new args) ,@(remove :method props :key #'car))
           (register-verb (function (setf ,name))))))))

(definline map-verbs (f)
  "Map F over *VERBS*."
  (mapcar f *verbs*))

(defun verbp (v)
  "Return T if V is the name of a verb."
  (when (member v *verbs* :key 'generic-function-name :test 'equal) t))

(defun setf-verbp (v)
  "Return T if V is a setf-able verb."
  (when (member `(setf ,v) *verbs* :key 'generic-function-name :test 'equal) t))

;; make-specializer-form-using-class
;; make-method-lambda-using-specializers
(defverb init (self &key &allow-other-keys)
  (:documentation "Intiailize SELF."))
(defverb start (self)
  (:documentation "Start object SELF."))
(defverb started-p (self)
  (:documentation "Return non-nil if object SELF has been started."))
(defverb stop (self &key &allow-other-keys)
  (:documentation "Stop object SELF."))
(defverb stopped-p (self)
  (:documentation "Return non-nil if object SELF has been stopped."))
(defverb shutdown (self)
  (:documentation "Shutdown object SELF."))
(defverb reset (self &rest args &key &allow-other-keys)
  (:documentation "Reset object SELF."))
(defverb data (self)
  (:accessor t)
  (:documentation "Return the data associated with SELF."))
(defverb head (self)
  (:accessor t)
  (:documentation "Return the head of SELF."))
(defverb tail (self)
  (:accessor t)
  (:documentation "Return the tail of SELF."))
(defverb name (self)
  (:accessor t)
  (:method ((self t))
    (string self))
  (:documentation "Return the name of object SELF."))
(defverb validate (obj self &key &allow-other-keys)
  (:documentation "Validate OBJ against SELF."))
(defverb status (self &key &allow-other-keys)
  (:documentation "Return the status of SELF."))
(defverb tags (self)
  (:documentation "Return the tags associated with object SELF."))
(defverb exec (self)
  (:documentation "Execute object SELF."))
(defverb explore (self &key &allow-other-keys)
  (:documentation "Explore object SELF."))
(defverb version (self)
  (:documentation "Return the version of object SELF."))
(defverb lock (self)
  (:documentation "Return the lock associated with SELF."))
(defverb upgrade (self)
  (:documentation "Return the upgrade-function associated with object SELF."))
(defverb bind (self)
  (:accessor t)
  (:documentation "Return the bindings associated with object SELF."))
(defverb assign (self assignee)
  (:documentation "Assign SELF to ASSIGNEE."))
(defverb copy (from to)
  (:documentation "Copy the contents of FROM into TO. Returns TO.")
  (:method :before ((x array) (y array))
    (assert (tree-equal (array-dimensions x) (array-dimensions y))
            nil 'dimension-mismatch))
  (:method ((from cons) (to cons))
    (do ((flst from (cdr flst))
         (tlst to (cdr tlst)))
        ((or (null flst) (null tlst)))
      (setf (car tlst) (car flst)))
    to)
  (:method ((from t) (to cons))
    (mapl #'(lambda (lst) (rplaca lst from)) to)
    to))
(defverb swap (from to)
  (:documentation "Swap the contents of FROM with the contents of TO, returning TO."))
(defverb call (self &rest args)
  (:documentation "Call SELF with ARGS."))

(defgeneric run-object (self &key &allow-other-keys)
  (:documentation "Explicitly run the object SELF."))

(defgeneric write-object (self stream &key &allow-other-keys)
  (:documentation "Write object SELF to STREAM.")
  (:method ((self t) (stream t) &key)
    (write self :stream stream)))

(defun slot-boundp* (self slot)
  "Return T if SLOT is bound in object SELF, otherwise return NIL."
  (when slot
    (handler-bind ((sb-pcl::missing-slot nil))
      (slot-boundp self slot))))

(defun shallow-copy-object (self)
  "Create a 'shallow' copy of object SELF."
  (let* ((class (class-of self))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'slot-definition-name (class-slots class)))
      (when (slot-boundp self slot)
        (setf (slot-value copy slot)
              (slot-value self slot))))
    copy))

(defgeneric copy-object (self)
  (:documentation "Return a copy of object SELF.")
  (:method ((self standard-object))
    (shallow-copy-object self)))

(defun list-indirect-class-methods (class)
  "List all indirect methods of CLASS."
  (remove-duplicates (mapcan #'specializer-direct-generic-functions (compute-class-precedence-list class))))

(defun list-class-methods (class methods &optional indirect)
  "List all methods specializing on CLASS modulo METHODS. When INDIRECT is
non-nil, also include indirect (parent) methods."
  (if (eq methods t)
      (if indirect
          (list-indirect-class-methods class)
          (specializer-direct-generic-functions class))
      (mapcar
       (lambda (s)
         (car (member s (specializer-direct-generic-functions class) :key #'generic-function-name)))
       methods)))

(defun list-class-slots (class slots &optional exclude)
  "List the SLOTS found in CLASS, optionally excluding list EXCLUDE."
  ;; should probably convert slot-definition-name here
  (let ((cs (remove-if
             (lambda (s)
               (or
                (null s)
                (member t (mapcar
                           (lambda (x)
                             (string= (slot-definition-name s) x))
                           exclude))))
             (class-slots class))))
    (if (eq slots t)
        cs
        (loop for s in slots
              with sn = (symb s)
              for c in cs
              with cn = (symb (slot-definition-name c))
              when (eq sn cn)
                collect c))))

(definline slot-values (obj &optional (slots (mapcar 'slot-definition-name (class-slots (class-of obj)))))
  "Returns a list containing slot-values of OBJ corresponding to symbols in the list SLOTS.

Example:
(defstruct obj a b)

(let ((thing (make-obj :a 1 :b 2)))
   (slot-values thing '(a b)))
;; (1 2)"
  (mapcar #'(lambda (s) (when (slot-boundp obj s) (slot-value obj s))) slots))

(defmacro with-fslots (slots instance &rest body)
  (with-gensyms (obj args)
    `(let ((,obj ,instance))
       (flet (,@(mapcar #'(lambda (decl)
                            (destructuring-bind (name slot-name) (if (consp decl) decl (list decl decl))
                              `(,name (&rest ,args) (apply (the function (slot-value ,obj ',slot-name)) ,args))))
                        slots))
         ,@body))))

;; TODO 2023-09-09: slot exclusion from dynamic var
(defvar *ignored-slots* nil
  "A list of slot names which may be ignored. See the function LIST-SLOT-VALUES-USING-CLASS.")

(defun list-slot-values-using-class (class obj slots &optional nullp unboundp)
  "List the values of SLOTS bound in OBJ according to CLASS. When NULLP is T also
include NIL values. Likewise with UNBOUNDP for unbound slot values."
  (remove-if
   #'null
   (mapcar
    (lambda (s)
      (let ((n (slot-definition-name s)))
        (let ((ns (make-keyword (symbol-name n))))
          (if (slot-boundp-using-class class obj s)
              (let ((v (slot-value-using-class class obj s)))
                (if nullp
                    `(,ns ,v)
                    (unless (null v)
                      `(,ns ,v))))
              (when unboundp (list ns))))))
    slots)))

(defmacro make-instance! (name &rest args)
  `(defmacro ,(intern (format nil "~:@(~a~)" name)) (,@args)
     (list 'make-instance '',name
           ,@(loop for i in args append `(,(intern (symbol-name i) :keyword) ,i)))))

(defmacro defclass! (name superclasses slots &rest options)
  "Helper for DEFCLASS forms. Automatically adds INITARG based on NAME."
  (let ((slots (loop for slot in slots 
                     collect 
                     (if (consp slot)
                         `(,(car slot) :initarg ,(sb-int:keywordicate (car slot)) ,@(cdr slot))
                         `(,slot :initarg ,(sb-int:keywordicate slot)))))
        (fun (find :auto options :test #'std/condition:car-eql))
        (meth (find :methods options :test #'std/condition:car-eql)))
    (when fun
      (setq options (remove :auto options :test #'std/condition:car-eql)
            fun (cadr fun)))
    (when meth
      (setq options (remove :methods options :test #'std/condition:car-eql)
            meth (cdr meth)))
    `(prog1
         (defclass ,name ,superclasses ,slots ,@options)
       (when ,fun
         (make-instance! ,name ,@(loop for s in slots collect (if (consp s) (car s) s))))
       ,@(when meth
           (mapcar (lambda (x) `(defmethod ,(car x) ,@(cdr x))) meth)))))

(defmacro defmethods (name &body forms)
  "Define multiple methods for a generic function. Each member of FORMS is passed
directly to a DEFMETHOD form."
  (eval-always
    `(progn
       ,@(loop for form in forms
               collect `(defmethod ,name ,@form)))))

(defmacro defaccessor (name-and-opts args &body expansion)
  "Define a pair of methods - an accessor with NAME and setf method for that accessor
which simply expands to: (SETF EXPANSION %VAL)."
  (let ((name (if (atom name-and-opts) name-and-opts (pop name-and-opts)))
        (type (if (atom name-and-opts) t (pop name-and-opts))))
    (eval-always
      `(progn
         (defmethod ,name ,args ,@expansion)
         (defmethod (setf ,name) ,(push `(new ,type) args) (setf ,@expansion new))))))

(defmacro defaccessor* (name args expansion setf-args &body setf-expansion)
    "Handle special case DEFACCESSOR forms. In higher-level packages we will
ocassionally have a more complex SETF expansion so here we support 2 additional arguments. 

The first specifies arguments for the SETF expansion in addition to the simple
accessor, and the second specifies the setf expansion.

Due to these changes the EXPANSION argument is downgraded from an &rest
argument."
  (eval-always
    `(progn
       (defmethod ,name ,args ,expansion)
       (defmethod (setf ,name) ,setf-args ,@setf-expansion))))
       
;; closer-mop
(defun ensure-finalized (class &optional (errorp t))
  "Ensure that CLASS is finalized returning an error if ERRORP is non-nil."
  (if (typep class 'class)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (when errorp (error "~S is not a class." class)))
  class)

(defun subclassp (class superclass)
  "Return T if CLASS is a subclass of SUPERCLASS."
  (flet ((get-class (class) (etypecase class
                              (class class)
                              (symbol (find-class class)))))

      (loop with class = (get-class class)
            with superclass = (get-class superclass)

            for superclasses = (list class)
            then (set-difference
                  (union (class-direct-superclasses current-class) superclasses)
                  seen)

            for current-class = (first superclasses)

            while current-class

            if (eq current-class superclass) return t
            else collect current-class into seen

            finally (return nil))))

(defun safe-superclasses (super classes)
  "Return a list of class symbols same as CLASSES if one of the members is a
subclass of SUPER."
  (if (find super classes :test (lambda (x y) (subclassp y x)))
      classes
      (push super classes)))
