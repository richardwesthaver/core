;;; std/meta.lisp --- Standard MOP Utilities

;;

;;; Code:
(in-package :std/meta)

;;; Verbs

;; Verbs are special generic-functions which we want to be able to perform
;; interesting operations on via OBJ/META/SEALED, OBJ/META/FAST, and more.
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

(defun verb-p (v)
  "Return T if V is the name of a verb."
  (when (member v *verbs* :key 'generic-function-name :test 'equal) t))

(defun verb-accessor-p (v)
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
(defverb save (self &rest args)
  (:documentation "Save according to spec SELF."))
(defverb stopped-p (self)
  (:documentation "Return non-nil if object SELF has been stopped."))
(defverb pause (self)
  (:accessor t)
  (:documentation "Pause the object SELF, in some instances also unpause when already paused."))
(defverb resume (self)
  (:documentation "Resume the object SELF from a paused state."))
(defverb shutdown (self)
  (:documentation "Shutdown object SELF."))
(defverb clean (self &key)
  (:documentation "Clean object SELF."))
(defverb purge (self &key)
  (:documentation "Purge object SELF."))
(defverb reset (self &rest args &key &allow-other-keys)
  (:documentation "Reset object SELF."))
(defverb resize (self size)
  (:documentation "Resize object SELF."))
(defverb data (self)
  (:accessor t)
  (:documentation "Return the data associated with SELF."))
(defverb head (self)
  (:accessor t)
  (:documentation "Return the head of SELF."))
(defverb tail (self)
  (:accessor t)
  (:documentation "Return the tail of SELF."))
(defverb timeout (self) ;; not exported to avoid conflict with SB-EXT::TIMEOUT (condition class)
  (:accessor t)
  (:documentation "Return the timeout of SELF."))
(defverb deadline (self) 
  (:accessor t)
  (:documentation "Return the deadline of SELF."))
(defverb name (self)
  (:accessor t)
  (:method ((self t))
    (string self))
  (:method ((self package))
    (package-name self))
  (:method ((self readtable))
    (readtable-name self))
  (:method ((self slot-definition))
    (slot-definition-name self))
  (:documentation "Return the name of object SELF."))
(defverb validate (obj self &key &allow-other-keys)
  (:documentation "Validate OBJ against SELF."))
(defverb state (self)
  (:accessor t)
  (:documentation "Return the state of SELF."))
(defverb stat (self &key)
  (:documentation "Return a short status message."))
(defverb tags (self)
  (:accessor t)
  (:documentation "Return the tags associated with object SELF."))
(defverb exec (self)
  (:documentation "Execute object SELF."))
(defverb explore (self &key &allow-other-keys)
  (:documentation "Explore object SELF.")
  (:method ((self string) &rest args)
    (std/print:print-table (std/list:group (apply 'ppcre:regex-apropos-list self args) 2)))
  (:method (self &rest args)
    (std/print:print-table (std/list:group (apply 'apropos-list self args) 2))))
(defverb version (self)
  (:accessor t)
  (:documentation "Return the version of object SELF."))
(defverb lock (self)
  (:accessor t)
  (:documentation "Return the lock associated with SELF."))
(defverb sync (self &key &allow-other-keys)
  (:documentation "Sync object SELF."))
(defverb upgrade (self)
  (:accessor t)
  (:documentation "Return the upgrade-function associated with object SELF."))
(defverb bind (self)
  (:accessor t)
  (:documentation "Return the binding associated with object SELF."))
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
(defverb clone (self)
  (:documentation "Return a clone of SELF."))
(defverb swap (from to)
  (:documentation "Swap the contents of FROM with the contents of TO, returning TO."))
(defverb call (self args)
  (:documentation "Call SELF with ARGS."))
(defverb install (self &key)
  (:documentation "Install object SELF."))
(defverb uninstall (self &key)
  (:documentation "Uninstall object SELF."))

(defverb build (self &key &allow-other-keys))
(defgeneric build-from (self from &key &allow-other-keys))

(definline init* (&rest keys)
  "Call the default initializer on each arg."
  (mapc 'init keys))

;; TODO 2025-11-01: 
(defverb scan (self seq &key))

(defverb send (self buffer &rest args &key start end))
(defverb receive (self &rest args &key buffer length start end &allow-other-keys))

;;; *-OBJECT
(defgeneric run-object (self &key &allow-other-keys)
  (:documentation "Explicitly run the object SELF."))

(defgeneric write-object (self stream &key &allow-other-keys)
  (:documentation "Write object SELF to STREAM.")
  (:method ((self t) (stream t) &key)
    (write self :stream stream)))

(defun slots-boundp (obj &rest slots)
  "Return T if all SLOTS are bound in OBJ."
  (dolist (slot slots t)
    (unless (slot-boundp obj slot)
      (return nil))))

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
    (shallow-copy-object self))
  (:method ((self hash-table))
    (std/hash:copy-hash self))
  (:method ((self list))
    (copy-list self))
  (:method ((self array))
    (std/array:copy-array self))
  (:method ((self symbol))
    (copy-symbol self))
  (:method ((self structure-object))
    (copy-structure self)))

;;; Struct Constructor
(defgeneric struct-constructor (class)
  (:documentation "Called to get the constructor name for a struct class. Users
                  should overload this when they want to serialize
                  non-standard constructor names. The default constructor
                  make-xxx will work by default. The argument is an eql style
                  type: i.e. of type (eql 'my-struct)"))

(defmethod struct-constructor ((class t))
  (symbol-function (intern (concatenate 'string "MAKE-" (symbol-name class))
                           (symbol-package class))))

;;; Helpers
(defun class-equalp (c1 c2)
  (when (symbolp c1) (setf c1 (find-class c1)))
  (when (symbolp c2) (setf c2 (find-class c2)))
  (eq c1 c2))

(defun type-specifier-and (&rest type-specifiers)
  (let ((relevant (remove t type-specifiers)))
    (cond ((null relevant) t)
          ((null (cdr relevant)) (first relevant))
          (t `(and ,@relevant)))))

(defun type-specifier-or (&rest type-specifiers)
  (let ((relevant (remove nil type-specifiers)))
    (cond ((null relevant) nil)
          ((null (cdr relevant)) (first relevant))
          (t `(or ,@relevant)))))

(defun type-specifier-not (type-specifier)
  (cond ((eql type-specifier t) nil)
        ((eql type-specifier nil) t)
        (t `(not ,type-specifier))))

(defparameter *standard-metaobjects*
  (list (find-class 'standard-object)
        (find-class 'standard-class)
        (find-class 'standard-generic-function)
        (find-class 'standard-method)
        (find-class 'built-in-class)))

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

(defun find-class-for-direct-slot (class def)
  (let ((list (sb-mop:compute-class-precedence-list class)))
    (labels ((rec (super)
               (if (null super)
                   nil
                   (if (find-direct-slot-def-by-name super (sb-mop:slot-definition-name def))
                       (class-name super)
                       (rec (pop list))))))
      (rec class))))

(defun find-direct-slot-def-by-name (class slot-name)
  (loop for slot-def in (sb-mop:class-direct-slots class)
        when (eq (sb-mop:slot-definition-name slot-def) slot-name)
        do (return slot-def)))

(defun find-slot-def-by-name (class slot-name)
  (loop for slot-def in (sb-mop:class-slots class)
        when (eq (sb-mop:slot-definition-name slot-def) slot-name)
        do (return slot-def)))

(defgeneric find-slot-defs-by-type (class type &optional by-subtype))
(defgeneric find-slot-def-names-by-type (class type &optional by-subtype))

(defun slots-and-values (o)
  "List of slot names followed by values for object"
  (loop for sd in (sb-mop:compute-slots (class-of o))
        for slot-name = (sb-mop:slot-definition-name sd)
        with ret = ()
        do
        (when (and (slot-boundp o slot-name)
                   (eq :instance
                       (sb-mop:slot-definition-allocation sd)))
          (push (slot-value o slot-name) ret)
          (push slot-name ret))
        finally (return ret)))

(defun struct-slots-and-values (object)
  "List of slot names followed by values for structure object"
  (let ((result nil)
        (slots 
          (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))))
    (loop for slot in slots do
         (push (slot-value object slot) result)
         (push slot result))
    result))

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

;;; Template Functions
;; Derived from MATLISP, this protocol provides a way to short-circuit the
;; standard method dispatch.

;; *TEMPLATE-TABLE* stores the global mapping of template-function names to a
;; plist with the following keywords:

;; LAMBDA-LIST - arguments to this function
;; PREDICATE - dispatch predicate
;; SORTER - sort predicate (defaults to predicate)
;; SORT-FUNCTION - sort function (defaults to TOPOSORT)

(defvar *template-table* (make-hash-table)
  "Global hash-table containing a mapping of template-function names to 'specs' (plists).")

(defun template-function-p (name)
  "Return Non-nil if NAME is a template-function, else return NIL. Value is
either T indicating a template-function without any template-methods or a
plist containing the template-function spec (plist)."
  (multiple-value-bind (val found) (gethash name *template-table*)
    (when found (or val t))))

(defgeneric compute-template-dispatch (name args)
  (:documentation "compute the dispatch return value of the template function NAME given lambda-list ARGS.")
  (:method ((name symbol) args)
    (let* ((data (or (gethash name *template-table*)
                     (error "undefined template : ~a~%" name)))
           (pred (getf data :predicate))
           (meth (getf data :methods)))
      (flet ((ffilter (pred)
               (loop named ff for ele in meth
                     do (when (funcall pred args (first ele))
                          (loop for (code . pp) in (cdr ele) 
                                do (when (or (not pp) (funcall pp args))
                                     (return-from ff code)))))))
        (or (ffilter #'equal)
            (ffilter #'(lambda (a m) (and (not (equal a m)) (funcall pred a m))))
            (error "could not find a \"~a\" template for : ~a~%" name args))))))

(defun single-arg-template-function-p (name)
  "Return T if NAME designates a template function which takes a single argument.

Also returns a second value of the lambda-list itself."
  (let* ((data (or (gethash name *template-table*)
                   (error "Undefined template : ~a~%" name)))
         (ll (getf data :lambda-list)))
    (values (not (consp (first ll))) ll)))

(defgeneric preprocess-template-dispatch (name args)
  (:documentation "Preprocess the template-function NAME by calling it with ARGS, which are
macroexpanded.")
  (:method ((name symbol) args)
    (funcall (if (single-arg-template-function-p name) #'funcall #'mapcar)
             #'macroexpand-1 args)))
;;
(defmacro define-template-generic ((name predicate &optional sorter (sort-function 'toposort)) disp args)
  "Define a template generic function stored in *TEMPLATE-TABLE*."
  (when (consp disp)
    (assert (null (remove-if-not #'(lambda (x) (member x cl:lambda-list-keywords)) disp)) nil "dispatch list contains keywords."))
  (with-gensyms (warg-sym disp-sym meth-sym pred-sym)
    (multiple-value-bind (disp-arg disp-far)
        (if (consp disp)
            (values `(&whole ,disp-sym ,@disp) disp-sym)
            (values disp disp))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *template-table*) (list :lambda-list (list ',disp ',args) :predicate ,predicate :sorter ,(or sorter predicate) :methods nil :sort-function ',sort-function))
         (defmacro ,name (&whole ,warg-sym ,disp-arg ,@args)
           (declare (ignore ,@(remove-if #'(lambda (x) (member x cl:lambda-list-keywords)) args) ,@(when (consp disp) disp)))
           (let* ((,pred-sym (preprocess-template-dispatch ',name ,disp-far))
                  (,meth-sym (compute-template-dispatch ',name ,pred-sym)))
             (apply ,meth-sym (cons ,pred-sym (cddr ,warg-sym)))))))))

(defmacro define-template-method (name disp args &rest body)
  "Define a template method for one of the pre-defined templates in *TEMPLATE-TABLE*."
  (with-gensyms (data-sym meth-sym afun-sym disp-sym sort-sym)
    (std/macs:letv* (((name &optional filter) (std/list:ensure-list name))
                     (data (or (gethash name *template-table*) (error "Undefined template : ~a~%" name)))
                     (ll (getf data :lambda-list))
                     (single? (not (consp (first ll))))
                     ;;
                     (disp-vars (funcall (if single? #'funcall #'mapcar) #'(lambda (x) (if (consp x) (car x) x)) disp))
                     (disp-spls (funcall (if single? #'funcall #'mapcar) #'(lambda (x) (if (consp x) (cadr x) t)) disp)))
      (assert (std/list:match-lambda-lists (list disp-vars args) ll) nil "mismatch in lambda-lists.")
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (let* ((,data-sym (or (gethash ',name *template-table*) (error "Undefined template : ~a~%" ',name)))
                (,meth-sym (getf ,data-sym :methods))
                (,afun-sym (lambda (,(if single? disp-vars disp-sym) ,@args)
                             (declare (ignorable ,@(remove-if #'(lambda (x) (member x cl:lambda-list-keywords))
                                                              (mapcar #'(lambda (x) (if (consp x) (car x) x))
                                                                      (cons (if single? disp-vars disp-sym) args)))))
                             ,(std/list:recursive-append
                               (unless single?
                                 `(destructuring-bind (,@disp-vars) ,disp-sym
                                    (declare (ignorable ,@disp-vars))))
                               `(locally ,@body))))
                (,sort-sym (getf ,data-sym :sorter)))
           (declare (ignorable ,data-sym ,meth-sym ,afun-sym ,sort-sym))
           (if-let ((lst (assoc ',disp-spls ,meth-sym :test #'equal)))
             (if-let ((flst (find ,filter (cdr lst) :key #'cdr)))
               (rplaca flst ,afun-sym)
               (rplacd lst (sort (list* (cons ,afun-sym ,filter) (cdr lst)) #'(lambda (a b) (or (cdr a) (not (cdr b)))))))
             (setf ,meth-sym (,(getf data :sort-function) (list* (list ',disp-spls (cons ,afun-sym ,filter)) ,meth-sym)
                              #'(lambda (a b) (funcall ,sort-sym (first a) (first b))))))
           (setf (getf ,data-sym :methods) ,meth-sym)
           ,afun-sym)))))

(defun remove-template-method (name spls)
  "Remove a template method for a generic function stored in *TEMPLATE-TABLE*,
given the name and specializer."
  (std/macs:letv* (((name &optional (filter '*)) (std/list:ensure-list name))
                   (data (or (gethash name *template-table*) (error "Undefined template : ~a~%" name)))
                   (meth (getf data :methods)))
    (if (eql filter '*)
        (setf (getf data :methods) (remove spls meth :test #'(lambda (a b) (equal a (first b)))))
        (when-let ((lst (find spls meth :test #'(lambda (a b) (equal a (first b))))))
          (rplacd lst (remove filter (cdr lst) :test #'(lambda (a b) (eql a (cdr b)))))))
    nil))

;;; Class Maps
;; inspired by death's dbus::define-name-class-mapping
(defmacro define-class-map (&key class map find)
  "Define an interface for mapping names (strings) to classes (or class names)."
  (let ((map-docstring (format nil "Map names to ~A classes or class names." class))
        (find-docstring (format nil "Return the ~A class (or class name) corresponding to NAME." class))
        (find-setf-docstring (format nil "Associate a ~A class (or class name) with NAME." class)))
    `(progn
       (defvar ,map
         (make-hash-table :test 'equal)
         ,map-docstring)
       (defun ,find (name &key (if-does-not-exist :error))
         ,find-docstring
         (or (gethash name ,map)
             (std/condition:missing-entry name if-does-not-exist)))
       (defun (setf ,find) (class name &key (if-exists :warn))
         ,find-setf-docstring
         (when-let ((old (,find name :if-does-not-exist nil)))
           (when (not (std/condition:replace-entry-p old class if-exists))
             (return-from ,find class)))
         (setf (gethash name ,map) class))
       ',class)))

;;; Sham Classes
;; inspired by CLX::DEF-CLX-CLASS (pseudo-class mechanism)
(defvar *sham-classes* nil
  "Control the behavior of the DEFSHAM macro.")

;; (defmacro defsham ((name &rest opts) &body slots))
