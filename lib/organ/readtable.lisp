;;; readtable.lisp --- Organ Readtable

;; 

;;; Code:
(in-package :organ)

(defun organ-reader (s sub num)
  "Parse a list of Org elements from stream S, returning an ORG-SECTION."
  (declare (ignore sub num))
  (let ((c (read-char s)))
    (org-parse :section
        (concatenate
         'string
         (loop while c
               until (and (char= c #\&) (char= (peek-char t s) #\#) (read-char s))
               collect c
               do (setf c (read-char s)))))))

(defreadtable :organ
  (:merge :std)
  (:dispatch-macro-char #\# #\& #'organ-reader))
