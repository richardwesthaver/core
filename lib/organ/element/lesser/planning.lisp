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

(defun planning-line-p (l) (scan org-planning-rx l))

(sb-int:defconstant-eqx +org-planning-keywords+ '("DEADLINE" "SCHEDULED" "CLOSED") #'equal)

;; helper object, not public API
(define-org-object planning-line ((keyword "" :type string) (timestamp "" :type string)))

;; always consume the string
(define-org-parser (planning-line :from stream)
  (when-let ((pos (file-position input))
             (l (read-line input nil nil)))
    ;; TODO 2025-03-11: don't scan twice
    (if (planning-line-p l)
        (multiple-value-bind (match subs)
            (scan-to-strings org-planning-rx l)
          (when match
            (let ((kw (aref subs 0))
                  (ts (aref subs 1)))
              (when (and kw ts)
                (let ((pl (org-create :planning-line)))
                  (setf (org-planning-line-keyword pl) kw
                        (org-planning-line-timestamp pl) ts)
                  pl)))))
        (progn
          (file-position input pos)
          nil))))

(define-org-element planning ((contents :initarg :contents :accessor org-contents :type (vector org-planning-line) :initform (make-array 0 :element-type 'org-planning-line :adjustable t :fill-pointer 0)))
  :lesser t)

(define-org-parser (planning :from stream)
  (when-let ((pl1 (org-parse :planning-line input)))
    (let ((p (org-create :planning)))
      (vector-push-extend pl1 (org-contents p))
      (loop with pl = (org-parse :planning-line input)
            until (not pl)
            do (vector-push-extend pl (org-contents p)))
      p)))
                                   
(define-org-parser (planning :from string)
  (with-input-from-string (s input)
    (org-parse :planning s)))
