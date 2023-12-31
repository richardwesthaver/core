;;; lib/organ/element/headline.lisp --- Org Headline

;; Headlines

;;; Code:

(in-package :organ)

(define-org-element todo-keyword
    ((todo-type :accessor todo-keyword-type :initarg :type :initform "" :type string)))

(define-org-parser (todo-keyword :from string)
  (when-let ((kw (org-todo-keyword-p input)))
    (org-create :todo-keyword :type kw)))

(define-org-element priority
  ((level :accessor org-priority-level :initarg :level :type character)))

(define-org-parser (priority :from string)
  (with-lexer-environment (input)
    (when (and (char= #\[ (consume))
               (char= #\# (consume))
               (not (char= #\] (peek))))
      (when-let ((c (consume)))
        (when (and (characterp (peek)) 
                   (char= #\] (peek))) ;; kludge
          (org-create :priority :level c))))))

(defun org-parse-todo-keyword-and-priority (input)
  "Parse INPUT returning the following values: 

(TODO-KEYWORD PRIORITY REST)"
  (let (kw prio rest)
    (multiple-value-bind (match subs)
        ;; scan for todo-keyword
        (scan-to-strings org-todo-keyword-rx input)
      (if match
	  (let ((k (aref subs 0)))
            (if-let ((%kw (org-parse :todo-keyword k)))
              (let* ((next (aref subs 1))
                     (prio? (org-parse :priority next)))
	        (setq kw %kw
                      prio prio?
                      rest (if prio? (trim (subseq next 4)) next)))
	      (setq rest (trim match))))
          ;; no kw found
          (let* ((next (trim input))
                 (prio? (org-parse :priority next)))
	    (setq kw nil ;; kw always comes before priority.
                  prio (org-parse :priority next)
                  rest (if prio? (subseq next 4) next)))))
    (values kw prio rest)))

(define-org-element tag
    ((name :initform "" :initarg :name :type string)))

(define-org-parser (tag :from string)
  (org-create type :name input))

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
                           (multiple-value-bind (kw prio title) 
                               (org-parse-todo-keyword-and-priority (subseq line rs))
                             (setf (hl-kw res) kw
                                   (hl-priority res) prio
                                   (hl-title res) title))))))
	  ;; scan for tags, modifies title slot
	  (let ((tag-str (cl-ppcre:scan-to-strings org-tag-rx (hl-title res))))
	    (when tag-str
	      (setf (hl-tags res) (apply #'vector (mapcar (lambda (x) (org-create :tag :name x)) (org-tag-split tag-str)))
                    ;;  Q 2023-12-27: should we preserve whitespace here?
		    (hl-title res) (string-right-trim 
                                    *whitespaces* 
                                    (subseq (hl-title res) 0 (- (length (hl-title res)) 1 (length tag-str)))))))))
      ;; TODO 2023-07-24: cookies,priority,comment,footnote,archive
      res)))
