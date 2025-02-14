;;; lib/organ/element/lesser/keyword.lisp --- Org Keyword Element

;; Keywords match the pattern '#+KEY: VALUE'

;; VALUE can be any of the standard-set of objects.

;; Affiliated keywords match the patterns:

#|
#+KEY: VALUE
#+KEY[OPTVAL]: VALUE
#+attr_BACKEND: VALUE
|#

;;; Code:
(in-package :organ)

(define-org-element keyword
    ((key :accessor keyword-key :initarg :key :type string)
     (val :accessor keyword-val :initarg :val))
  :lesser t)

(define-org-parser (keyword :from string)
  (multiple-value-bind (match-start match-end start end) (scan org-file-property-rx input)
    (declare (ignore match-end))
    (when match-start
      (let ((key (subseq input (aref start 0) (aref end 0)))
            (val (subseq input (aref start 1) (aref end 1))))
        (if (and (< 7 (length key)) (string= "COMMENT" (string-upcase (subseq key 0 7))))
            (org-create :comment :contents val)
            (org-create :keyword :key key :val val))))))

(define-org-element affiliated-keyword (key opt value) :lesser t)

(define-org-parser (affiliated-keyword :from string))
