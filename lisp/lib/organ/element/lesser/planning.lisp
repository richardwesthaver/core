;;; lib/organ/element/lesser/planning.lisp --- Org Planning Elements

;; A planning element matches the pattern:

#|
HEADING
PLANNING
|#

;; HEADING is just a heading. 

;; PLANNING matches the pattern: 'KEYWORD: TIMESTAMP'.

;; KEYWORD is one of DEADLINE, SCHEDULED, CLOSED.

;;; Code:
(in-package :organ)

(sb-int:defconstant-eqx +org-planning-keywords+ '("DEADLINE" "SCHEDULED" "CLOSED") #'equal)

;; helper object, not public API
(define-org-object planning-line ((keyword "" :type string) (timestamp "" :type string)))

;; always consume the string
(define-org-parser (planning-line :from string)
  (multiple-value-bind (match subs)
      (scan-to-strings org-planning-rx input)
    (when match
      (let ((kw (aref subs 0))
            (ts (aref subs 1)))
        (when (and kw ts)
          (let ((pl (org-create :planning-line)))
            (setf (org-planning-line-keyword pl) kw
                  (org-planning-line-timestamp pl) ts)
            pl))))))

(define-org-element planning ((contents :initarg :contents :accessor org-contents :type (vector org-planning-line) :initform (make-array 0 :element-type 'org-planning-line :adjustable t :fill-pointer 0)))
  :lesser t)

(define-org-parser (planning :from stream)
  (let ((first (read-line input nil :eof))
        (p (org-create :planning)))
    (when-let ((pl1 (org-parse :planning-line first)))
      (vector-push-extend pl1 (org-contents p))
      (loop for l = (peek-line input)
            until (not l)
            with pl = (org-parse :planning-line (read-line input))
            until (not pl)
            do (vector-push-extend pl (org-contents p)))
      p)))
                                   
(define-org-parser (planning :from string)
  (with-input-from-string (s input)
    (org-parse :planning s)))
