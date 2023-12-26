;;; lib/organ/obj.lisp --- Organ Elements

;;

;;; Code:
(in-package :organ)

;;; Headline
;; when level=0, headline is uninitialized
(defclass org-headline (org-element)
  ((state :initarg :state :accessor state :initform nil)
   (level :initarg :level :accessor level :initform 0)
   (priority :initarg :priority :accessor priority :initform nil)
   (tags :initarg :tags :accessor tags :initform nil)
   (title :initarg :title :accessor title :initform ""))
  (:documentation "Org Headline object without connection to other
  elements. This is a deviation from the org-element specification in
  the name of utility. Properties, Logbook, and Body objects are
  defined separately too, so a complete Heading object can be
  summarized as a list of at most four elements: The headline,
  properties, logbook and body."))

(defmethod org-create ((type (eql :headline)) text &key state (level 1) priority tags (title ""))
  (%make-it type text :state state :level level :priority priority :tags tags :title title))

(defmethod org-parse ((self org-headline))
  (with-input-from-string (s (text self))
    ;; first we parse 'just' the headline
    (when (peek-char #\* s) 
      (let ((line (read-line s)))
	(multiple-value-bind (start _ reg-start reg-end)
	    ;; scan for headline
	    (cl-ppcre:scan org-headline-rx line)
	  (declare (ignore _))
	  (when start
	    (loop for rs across reg-start
		  for re across reg-end
		  for i from 0
		  do
		     (if (= i 0)
			 (setf (level self) (- re rs))
			 (let ((sub (subseq line rs)))
			   (multiple-value-bind (match subs)
			       ;; scan for todo-keyword
			       (cl-ppcre:scan-to-strings org-todo-keyword-rx sub)
			       (if match
				   (let ((k (svref subs 0)))
				     (if (org-todo-keyword-p k)
					 (setf (state self) (make-org-todo-keyword k)
					       (title self) (trim (svref subs 1)))
					 (setf (title self) match)))
				   (setf (title self) sub))))))))
	;; scan for tags, modifies title slot
	(let ((tag-str (cl-ppcre:scan-to-strings org-tag-rx (title self))))
	  (when tag-str
	    (setf (tags self) (apply #'vector (mapcar #'make-org-tag (org-tag-split tag-str)))
		  (title self) (subseq (title self) 0 (- (length (title self)) 1 (length tag-str))))))))
  ;; TODO 2023-07-24: cookies,priority
  self))

;;; Todo Keyword
(defclass org-todo-keyword (org-element)
  ((todo-type :accessor todo-type :initarg :type :initform nil :type symbol)))

(defmethod org-create ((type (eql :todo-keyword)) text &key todo-type)
  (%make-it type text :todo-type todo-type))

(defmethod org-parse ((self org-todo-keyword))
  (let* ((text (text self))
	 (type (gethash (intern text) org-todo-keyword-map nil)))
    (when type (setf (todo-type self) type))
    self))

;;; List
(defclass org-list (org-element) ())

;;; Tag
(defclass org-tag (org-element) ())
  
(defmethod org-create ((type (eql :tag)) text &key &allow-other-keys)
  (%make-it type text))

(defmethod org-parse ((self org-tag)) self) ;; nop

;;; Block
(defclass org-block (org-element) ())

;;; Paragraph
(defclass org-paragraph (org-element) ())
