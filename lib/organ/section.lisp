;;; lib/organ/section.lisp --- Org Sections

;;

;;; Code:
(in-package :organ)

(defclass org-section () 
  ((contents :initform #() :initarg :contents :type (vector org-object)
             :accessor org-contents)))

(defmethod org-create ((type (eql :section)) &rest initargs)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(defmethod org-parse ((type (eql :section)) (input string))
  (unless (sequence:emptyp input)
    (org-create :section :contents input)))

(defclass org-meta-section (org-section) ((keywords :initform #() :initarg :keywords :type (vector org-keyword))))

(defmethod org-create ((type (eql :meta)) &rest initargs)
  (apply #'make-instance 'org-meta-section initargs))

(defmethod org-parse ((type (eql :meta)) (input string))
  (unless (sequence:emptyp input)
    (let ((contents (make-array 0 :element-type 'org-element :fill-pointer 0 :adjustable t)))
      (log:debug! "meta section input:" input)
      (with-input-from-string (s input)
        (vector-push-extend (org-parse :keyword (read-line s)) contents))
      (org-create :meta :contents contents))))

(defmethod org-parse ((type (eql :meta)) (input stream))
  (let ((keywords (make-array 0 :element-type 'org-keyword :adjustable t :fill-pointer 0))
        (content (make-array 0 :element-type 'character :fill-pointer 0)))
    (with-output-to-string (content-stream content)
      (loop for c = (peek-char nil input nil nil) ; check that this line isn't a headline
            until (or (not c) (char= #\* c))
            do (let ((l (read-line input)))
                 (if-let ((kw (org-parse :keyword l))) ;; comments are handled here
                   (unless (typep kw 'org-comment)
                     (vector-push-extend kw keywords))
                   (write-line l content-stream)))))
    (org-create :meta :keywords keywords :contents (org-parse :paragraph content))))
