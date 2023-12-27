;;; lib/organ/proto.lisp --- Organ Protocol

;;

;;; Code:
(in-package :organ)

(defclass org-stream (fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))

(defclass org-element () ())

(defclass org-lesser-element (org-element) ())

(defclass org-greater-element (org-element)
  ((contents :initarg :contents :type (vector org-element))))

(defclass org-object () ())

(defgeneric org-parse (type input)
  (:documentation "Parse string INPUT as an org element of type TYPE."))

(defgeneric org-parse-lines (type input)
  (:documentation "Convenience method. Parse INPUT as a vector
of lines, returning it. Each line object is a cons cell where car is a
keyword and cdr is the raw text parsed.")
  (:method ((type (eql t)) (input string))
    (let ((lines (read-org-lines-from-string input)))
      (remove-if-not 
       #'consp
       (loop for x across lines
	     collect
             (cond 
               ((scan org-headline-rx x) (cons :headline x))
               ((scan org-file-property-rx x) (cons :file-property x))
               ((scan org-property-start-rx x) (continue))
               ((scan org-property-rx x) (cons :node-property x))
               ((scan org-end-rx x) (continue))
               (t x)))))))

(defgeneric org-create (type &rest initargs)
  (:documentation "Create a new org-element of type TYPE.")
  (:method ((type t) &rest initargs)
    (apply #'make-instance (kw->class type) initargs)))

(defgeneric org-push (elt place)
  (:documentation "Add org-element ELT to object PLACE.")
  (:method ((elt org-element) (place sequence))
    (push elt place)))

(defgeneric org-write (elt stream)
  (:documentation "Write org-element ELT to output STREAM.")
  (:method ((elt org-element) stream))) ;; no-op

(defgeneric org-contents (elt)
  (:documentation "Extract contents from org-element ELT."))

(defgeneric (setf org-contents) (elt contents)
  (:documentation "Set ELT's contents to CONTENTS. Return ELT."))

(defgeneric org-property (elt prop)
  (:documentation "Extract the value from property PROP of org-element ELT."))

(defgeneric (setf org-property) (elt prop val)
  (:documentation "In org-element ELT set PROP to VAL. Return modified org-element."))

(defgeneric org-get-element (elt place)
  (:documentation "Extract org-element ELT from sequence PLACE."))

(defgeneric (setf org-get-element) (old new place)
  (:documentation "Replace OLD with NEW in sequence of org-elements at PLACE."))

(defgeneric org-insert-before (elt location place)
  (:documentation "Insert org-element ELT before LOCATION in sequence PLACE. Modify PLACE
by side-effect."))

(defgeneric org-parse-minimal (input)
  (:documentation "Parse the minimal set of objects as defined by Org syntax.

The minimal set includes the symbols defined in +ORG-MINIMAL-OBJECTS+."))

(defgeneric org-parse-standard (input)
  (:documentation "Parse the standard set of object as define by Org syntax.

The standard set includes the symbols defined in +ORG-STANDARD-OBJECTS+."))
