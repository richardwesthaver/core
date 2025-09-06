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

(defun planning-line-scan (l) (scan org-planning-rx l))

(define-constant +org-planning-keywords+ '("DEADLINE" "SCHEDULED" "CLOSED") :test #'equal)

;; helper object, not public API
(define-org-object planning-line ((keyword "" :type string) (timestamp "" :type string)))

;; always consume the string
(define-org-parser (planning-line :from stream)
  (when-let ((pos (file-position input))
             (l (read-line input nil nil)))
    (multiple-value-bind (start end) (planning-line-scan l)
      (if start
          (let ((kw (subseq l start end))
                (pl (org-create :planning-line)))
            (multiple-value-bind (_ .end .rbeg .rend) (scan org-timestamp-rx (subseq l end))
              (declare (ignore _))
              (when .rbeg
                (let* ((..end (aref .rend 0)))
                  (setf (org-planning-line-keyword pl) kw
                        (org-planning-line-timestamp pl) (subseq l (aref .rbeg 0) ..end))
                  (if (eql ..end .end)
                      (let ((rest (trim (subseq l ..end))))
                        (if (zerop (length rest))
                            pl
                            (vector
                             pl
                             (with-input-from-string (s rest) (org-parse :planning-line s))))))))))
          (progn
            (file-position input pos)
            nil)))))

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
