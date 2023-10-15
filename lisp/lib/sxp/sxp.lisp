;;; sxp.lisp --- S-eXPressions

;; sxp is a unified S-Expression data format

;;; Code:
(pkg:defpkg :sxp
    (:use :cl :sb-mop :sym :fu)
  (:import-from :uiop :read-file-forms :slurp-stream-forms :with-output-file)
  ;; TODO: hot-patch readtables into sxp classes/parsers
  (:import-from :readtables :defreadtable :in-readtable)
  (:export
   :sxp-fmt-designator
   :form :formp :sxp-error :sxp-fmt-error :sxp-syntax-error :reader :writer :fmt
   :wrap :wrap! :wrap-from-string! :unwrap :unwrap! :unwrap-or
   :sxpp :build-ast :load-ast :ast
   :define-macro :define-fmt :read-sxp-file :write-sxp-file
   :read-sxp-string :write-sxp-string :read-sxp-stream :write-sxp-stream
   :make-sxp :sxp :formp :form
   :wrap-object :unwrap-object))

(in-package :sxp)

(defun formp (form)
  (or (consp form) (atom form)))

(deftype form ()
  '(satisfies formp))

;;; Conditions
(define-condition sxp-error (error) ())

(define-condition sxp-fmt-error (sxp-error)
  ((format-control :initarg :format-control :reader format-control)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c s)
             (apply 'format s (format-control c) (format-arguments c)))))

(define-condition sxp-syntax-error (sxp-error) ())

 ;;; Protocol
(defgeneric wrap (self form))
(defgeneric wrap! (self form))
(defgeneric wrap-from-string! (self str))
(defgeneric unwrap (self))
(defgeneric unwrap! (self))
(defgeneric unwrap-or (self lambda))
(defgeneric sxpp (self form))

(defgeneric write-sxp-stream (self stream &key pretty case))
(defgeneric read-sxp-stream (self stream))

(defgeneric build-ast (self &key &allow-other-keys)
  (:documentation "build the sxp representation of SELF and store it in the :ast
slot. The :ast slot is always ignored."))

(defgeneric load-ast (self)
  (:documentation "load the object SELF from the :ast slot."))

;;; Objects
(defclass sxp ()
  ((ast :initarg :ast :type form :accessor ast))
  (:documentation "Dynamic class representing a SXP form."))

(defmethod wrap! ((self sxp) form) (setf (slot-value self 'ast) (ignore-errors form)))

(defmethod wrap-from-string! ((self sxp) str) (setf (slot-value self 'ast) (ignore-errors (read str))))

(defmethod wrap ((self sxp) form) (setf (slot-value self 'ast) form))

(defmethod unwrap ((self sxp)) (slot-value self 'ast))

(defmethod unwrap! ((self sxp)) (ignore-errors (slot-value self 'ast)))

(defmethod unwrap-or ((self sxp) else-fn)
  (if (slot-unbound 'sxp self 'ast)
      (slot-value self 'ast)
      (if (null (slot-value self 'ast))
	  (funcall else-fn))))

(defmethod write-sxp-stream ((self sxp) stream &key (pretty *print-pretty*) (case :downcase))
  (write (ast self)
	 :stream stream
	 :pretty pretty
	 :case case))

(defmethod read-sxp-stream ((self sxp) stream)
  (setf (ast self) (slurp-stream-forms stream :count nil)))

;; (defsetf unwrap ) (defsetf wrap )

;;; Functions
(defun read-sxp-file (file)
  (make-instance 'sxp :ast (read-file-forms file)))

(defun write-sxp-file (sxp file &optional &key if-exists)
  (with-output-file (out file) :if-exists if-exists
    (write-sxp-stream sxp out)))

(defun read-sxp-string (self str) (with-input-from-string (s str) (read-sxp-stream self s)))

(defun write-sxp-string (sxp) 
  (let ((ast (ast sxp)))
    (if (> (length ast) 1)
	(write-to-string ast)
	(write-to-string (car ast)))))

(defun make-sxp (&rest form) (make-instance 'sxp :ast form))

(deftype sxp-fmt-designator () '(member :canonical :collapsed)) 

(defun unwrap-object (obj &key (slots t) (methods nil)
			    (indirect nil) (tag nil)
			    (unboundp nil) (nullp nil)
			    (exclude nil))
  "Build and return a new `form' from OBJ by traversing the class
definition. This differs from the generic function `unwrap' which
always uses the ast slot as an internal buffer. We can also call this
on any class instance (doesn't need to subclass `sxp').

SLOTS specifies the slots to be included in the output. If the value
is t, all slots are included. The ast slot is not included by default,
but this behavior may change in future revisions.

When INDIRECT is non-nil, also include methods which indirectly
specialize on OBJ.

When TAG is non-nil, return a cons where car is TAG and cdr is the
output. If TAG is t, use the class-name symbol."
  (declare (type standard-object obj)
	   (type (or list boolean) slots)
	   (type (or list boolean) methods)
	   (type boolean indirect)
	   (type list exclude))
  (unless (or slots methods)
    (error "Required one missing key arg: SLOTS or METHODS"))
  (let* ((class (class-of obj))
	(res (when tag (list (if (eq t tag) (class-name class) tag)))))
    (block unwrap
      (when-let ((slots (when slots
			  (list-class-slots class slots exclude))))
	(let ((slot-vals (list-slot-values-using-class class obj (remove-if #'null slots) nullp unboundp)))
	  (if methods
	      (push slot-vals res)
	      (return-from unwrap (push slot-vals res)))))
      (when-let ((methods (when methods (list-class-methods class methods indirect))))
	(push methods res)))
    (flatten res)))

(defun wrap-object (class form)
  "Given a CLASS prototype and an input FORM, return a new instance of
CLASS. FORM is assumed to be the finalized lisp object which has
already passed through `read' -- not a string or file-stream for
example."
  (declare (type class class)
	   (type form form)))

;; (defmacro define-fmt ())
;; (defmacro define-macro ())
