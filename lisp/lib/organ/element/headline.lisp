;;; lib/organ/element/headline.lisp --- Org Headline

;; Headlines

;;; Code:

;;; Todo Keyword
(in-package :organ)

(define-org-element todo-keyword
    ((todo-type :accessor todo-type :initarg :type :initform nil :type symbol)))

(defmethod org-parse ((type (eql :todo-keyword)) (input string))
  (org-create :todo-keyword :todo-type (gethash (intern input) org-todo-keyword-map nil)))

;;; Tag
(define-org-element org-tag
    ((name :initform "" :initarg :name :type string)))

(defmethod org-parse ((type (eql :tag)) input) (org-create type :name input))

;;; Headline
;; when level=0, headline is uninitialized
(define-org-element headline
    ((stars :initarg :stars :accessor hl-stars :initform 0)
     (keyword :initarg :kw :accessor hl-kw :initform nil)
     (priority :initarg :priority :accessor hl-priority :initform nil)
     (title :initarg :title :accessor hl-title :initform "")
     (tags :initarg :tags :accessor hl-tags :initform nil))
  :documentation "Org Headline object without connection to other
  elements. This is a deviation from the org-element specification in
  the name of utility. Properties, Logbook, and Body objects are
  defined separately too, so a complete Heading object can be
  summarized as a list of at most four elements: The headline,
  properties, logbook and body.")

(defmethod org-parse ((type (eql :headline)) (input string))
  (let ((res (org-create type)))
    (with-input-from-string (s input)
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
			   (setf (hl-stars res) (- re rs))
			   (let ((sub (subseq line rs)))
			     (multiple-value-bind (match subs)
			         ;; scan for todo-keyword
			         (cl-ppcre:scan-to-strings org-todo-keyword-rx sub)
			       (if match
				   (let ((k (svref subs 0)))
				     (if (org-todo-keyword-p k)
					 (setf (hl-kw res) (org-create :todo-keyword :todo-type k)
					       (hl-title res) (trim (svref subs 1)))
					 (setf (hl-title res) match)))
				   (setf (hl-title res) sub))))))))
	  ;; scan for tags, modifies title slot
	  (let ((tag-str (cl-ppcre:scan-to-strings org-tag-rx (hl-title res))))
	    (when tag-str
	      (setf (hl-tags res) (apply #'vector (mapcar (lambda (x) (org-create :tag :name x)) (org-tag-split tag-str)))
		    (hl-title res) (subseq (hl-title res) 0 (- (length (hl-title res)) 1 (length tag-str))))))))
      ;; TODO 2023-07-24: cookies,priority,comment,footnote,archive
      res)))
