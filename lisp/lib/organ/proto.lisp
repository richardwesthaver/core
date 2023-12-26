;;; lib/organ/proto.lisp --- Organ Protocol

;;

;;; Code:
(in-package :organ)

(defclass org-stream (fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))

(defclass org-file (org-element org-stream)
  ((path :initarg :path :accessor path)))

(defclass org-lines (org-element)
  ((lines :initarg :lines :type vector :accessor o-lines)))

;;; Helpers
(defun read-org-file (path)
  (make-instance 'org-file :path path :text (read-file-string path)))

(defun read-org-lines (&optional stream)
  (let ((slice (make-instance 'org-lines)))
    (setf (o-lines slice)
	  (apply #'vector
		 (loop for l = (read-line stream nil :eof)
		       until (eq l :eof)
		       collect l)))
    slice))

(defun read-org-lines-from-string (str)
  (with-input-from-string (s str) (read-org-lines s)))

(defclass org-element ()
  ((text :initarg :text :accessor text :type string)))

(defgeneric org-parse (self)
  (:documentation "Parse the text slot from ORG-ELEMENT."))

(defgeneric org-parse-lines (self)
  (:documentation "Parse the text slot from ORG-ELEMENT as a vector of lines, returning
it. Each line object is a cons cell where car is a keyword and cdr is
the raw text parsed.")
  (:method ((self org-element))
    (let ((lines (o-lines (read-org-lines-from-string (slot-value self 'text)))))
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

(defun kw->class (kw) 
  "Convert keyword KW to a symbol which could designate an ORG- object type."
  (symbolicate 'org- kw))
  
(defun %make-it (type text &rest slots)
  (apply #'make-instance (kw->class type) (nconc (list :text text) slots)))

(defgeneric org-create (elt text &key &allow-other-keys)
  (:documentation "Create a new org-element of type TYPE.")
  (:method (elt (text string) &key &allow-other-keys)
    (org-parse-lines (%make-it elt text))))

(defgeneric org-push (elt place)
  (:documentation "Add org-element ELT to object PLACE.")
  (:method ((elt org-element) (place sequence))
    (push elt place)))

(defgeneric org-write (elt stream)
  (:documentation "Write org-element ELT to output STREAM.")
  (:method ((elt org-element) stream))) ;; no-op

(defgeneric org-contents (elt)
  (:documentation "Extract contents from org-element ELT."))

(defgeneric (setf org-contents) (elt text)
  (:documentation "Set ELT's contents (text) to CONTENT. Return ELT."))

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
