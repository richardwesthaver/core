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
    ((key :accessor name :initarg :key :type string)
     (val :accessor val :initarg :val))
  :lesser t)

(define-org-parser (keyword :from string)
  (multiple-value-bind (match-start match-end start end) (scan org-file-property-rx input)
    (declare (ignore match-end))
    (when match-start
      (let ((key (subseq input (aref start 0) (aref end 0)))
            (val (subseq input (aref start 1) (aref end 1))))
        ;; handle comments
        (string-case ((string-upcase key) :default (org-create :keyword :key key :val val))
          ("COMMENT" (org-create :comment :contents val))
          ("BEGIN" 
           (with-input-from-string (s val)
             (let ((name (read s))
                   (params (read-lisp-until-end s)))
               (org-create :dynamic-block :name name :parameters params)))))))))

(define-org-element affiliated-keyword (key opt value) :lesser t)

(define-org-parser (affiliated-keyword :from string))
