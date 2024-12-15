;;; std/meta.lisp --- Standard MOP Utilities

;;

;;; Code:
(in-package :std/meta)

;; make-specializer-form-using-class
;; make-method-lambda-using-specializers

(defgeneric start (self))
(defgeneric started-p (self))
(defgeneric stop (self &key &allow-other-keys))
(defgeneric stopped-p (self))
(defgeneric shutdown (self))
(defgeneric reset (self &rest args &key &allow-other-keys))
(defgeneric data (self))
(defgeneric name (self)
  (:method ((self t))
    (string self)))
(defgeneric tags (self))
(defgeneric write-object (obj stream &key &allow-other-keys)
  (:method ((obj t) (stream t) &key)
    (write obj :stream stream)))

(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'slot-definition-name (class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

(defgeneric copy-object (self)
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

;; TODO 2023-09-09: slot exclusion from dynamic var
(defun list-slot-values-using-class (class obj slots &optional nullp unboundp)
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
        (fun (member :auto options :test #'std/condition::car-eql)))
    (when fun
      (setq options (remove (car fun) options))
      (setq fun (cadar fun)))
    `(prog1
         (defclass ,name ,superclasses ,slots ,@options)
       (when ',fun
         (make-instance! ,name ,@(loop for s in slots collect (if (consp s) (car s) s)))))))

(defmacro defmethods (name &body forms)
  "Define multiple methods for a generic function. Each member of FORMS is passed
directly to a DEFMETHOD form."
  (eval-always
    `(progn
       ,@(loop for form in forms
               collect `(defmethod ,name ,@form)))))

(defmacro defaccessor ((name &optional (type 't)) args &body expansion)
  "Define a pair of methods - an accessor with NAME and setf method for that accessor
which simply expands to: (SETF EXPANSION %VAL)."
  (eval-always
    `(progn
       (defmethod ,name ,args ,@expansion)
       (defmethod (setf ,name) ,(push `(new ,type) args) (setf ,@expansion new)))))

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
  (if (typep class 'class)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (when errorp (error "~S is not a class." class)))
  class)

(defun subclassp (class superclass)
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
