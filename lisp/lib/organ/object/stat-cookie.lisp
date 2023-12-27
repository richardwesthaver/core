;;; Code:
(in-package :organ)

;; the logic will be a bit weird here - we store 2 numbers (completed
;; vs remaining) but sometimes need to parse a percentage without
;; actually knowing the counts of completed vs remaining. To get
;; around this, we'll allow a float to be stored in the N1 slot, which
;; indicated that we parsed a percentage without knowing our counts.
(define-org-object stat-cookie ((n1 0 :type (or fixnum float)) (n2 0 :type fixnum)))

(defmacro matches (name)
  `(make-matcher ,name))

(define-matcher stat-cookie-start (and (is #\[)
                                       (next (or (in #\0 #\9)
                                                 (is #\/)
                                                 (is #\%)))))

(define-matcher stat-cookie-end (is #\]))

(define-matcher stat-cookie-percent (is #\%))

(define-matcher stat-cookie-ratio (is #\/))

(define-org-parser (stat-cookie :from string)
  ;; either X/Y or X%
  (with-lexer-environment (input)
    (when (matches :stat-cookie-start)
      (consume)
      (let ((res (org-create :stat-cookie)))
        (setf (org-stat-cookie-n1 res)
              (parse-number
               (consume-until (matches (or :stat-cookie-percent
                                           :stat-cookie-ratio)))))
        (consume)
        (when (matches :stat-cookie-ratio)
          (setf (org-stat-cookie-n2 res) (parse-number (consume-until (matches :stat-cookie-end)))))
        res))))

