;;; Code:
(in-package :organ)

;; the logic will be a bit weird here - we store 2 numbers (completed
;; vs remaining) but sometimes need to parse a percentage without
;; actually knowing the counts of completed vs remaining. To get
;; around this, we'll allow a float to be stored in the N1 slot, which
;; indicated that we parsed a percentage without knowing our counts.
(define-org-object stat-cookie ((n1 0 :type number) (n2 0 :type fixnum)))

(defmacro matches (name)
  `(make-matcher ,name))

(define-matcher stat-cookie-percent (is #\%))

(define-matcher stat-cookie-ratio (is #\/))

(define-matcher int (in #\0 #\9))

(define-matcher stat-cookie-start 
    (and (is #\[)
         (next (matches (or :int
                            :stat-cookie-ratio
                            :stat-cookie-percent)))))

(define-matcher stat-cookie-end (is #\]))

;; this feels slow
(define-org-parser (stat-cookie :from string)
  ;; either X/Y or X%
  (with-lexer-environment (input)
    (when (char= #\[ (consume))
      (let ((res (org-create :stat-cookie)))
        (setf (org-stat-cookie-n1 res)
              (parse-number
               (consume-until (matches (not :int)))))
        (case (consume)
          (#\/ (setf (org-stat-cookie-n2 res) (parse-number (consume-until (matches :stat-cookie-end)))))
          (#\% (setf (org-stat-cookie-n1 res) (/  (org-stat-cookie-n1 res) 100))))
        res))))

