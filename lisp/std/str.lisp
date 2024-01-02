;;; std/str.lisp --- String utilities

;;; Code:

;; (defvar sb-unicode-syms 
;;   '(words lines sentences whitespace-p uppercase lowercase titlecase
;;     word-break-class line-break-class sentence-break-class char-block
;;     cased-p uppercase-p lowercase-p titlecase-p casefold
;;     graphemes grapheme-break-class
;;     bidi-mirroring-glyph bidi-class
;;     normalize-string normalized-p default-ignorable-p
;;     confusable-p hex-digit-p mirrored-p alphabetic-p math-p
;;     decimal-value digit-value
;;     unicode< unicode> unicode= unicode-equal
;;     unicode<= unicode>=))
(in-package :std)

;; (mapc (lambda (s) (export s)) sb-unicode-syms)
;; (reexport-from 
;;  :sb-unicode
;;  :include sb-unicode-syms)

(defparameter *omit-nulls* nil)
(defvar *whitespaces* (list #\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page
                            #\Return #\Space #\Rubout
                            #+sbcl #\Next-Line #-sbcl (code-char 133)
                            #\No-break_space)
  "On some implementations, linefeed and newline represent the same character (code).")

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

;; (defun split (separator s &key (omit-nulls *omit-nulls*) limit (start 0) end)
;;   "Split s into substring by separator (cl-ppcre takes a regex, we do not).

;;   `limit' limits the number of elements returned (i.e. the string is
;;   split at most `limit' - 1 times)."
;;   ;; cl-ppcre:split doesn't return a null string if the separator appears at the end of s.
;;   (let* ((limit (or limit (1+ (length s))))
;;          (res (cl-ppcre:split separator s :limit limit :start start :end end)))
;;     (if omit-nulls
;;         (remove-if (lambda (it) (sequence:emptyp it)) res)
;;         res)))

(defun collapse-whitespaces (s)
  "Ensure there is only one space character between words.
  Remove newlines."
  (cl-ppcre:regex-replace-all "\\s+" s " "))

(defun trim (s &key (char-bag *whitespaces*))
  "Removes all characters in `char-bag` (default: whitespaces) at the beginning and end of `s`.
   If supplied, char-bag has to be a sequence (e.g. string or list of characters).

   Examples: (trim \"  foo \") => \"foo\"
             (trim \"+-*foo-bar*-+\" :char-bag \"+-*\") => \"foo-bar\"
             (trim \"afood\" :char-bag (str:concat \"a\" \"d\")) => \"foo\""
  (when s
    (string-trim char-bag s)))

;;;  TODO 2023-08-27: camel snake kebab

(defun make-template-parser (start-delimiter end-delimiter &key (ignore-case nil))
  "Returns a closure than can substitute variables
  delimited by \"start-delimiter\" and \"end-delimiter\"
  in a string, by the provided values."
  (check-type start-delimiter string)
  (check-type end-delimiter string)
  (when (or (string= start-delimiter "")
            (string= end-delimiter ""))
      (error 'simple-type-error
              :format-control "The empty string is not a valid delimiter."))
  (let ((start-len (length start-delimiter))
        (end-len (length end-delimiter))
        (test (if ignore-case
                  #'string-equal
                  #'string=)))

    (lambda (string values)
      (check-type string string)
      (unless (listp values)
        (error 'simple-type-error
               :format-control "values should be an association list"))

      (with-output-to-string (stream)
        (loop for prev = 0 then (+ j end-len)
              for i = (search start-delimiter string)
                      then (search start-delimiter string :start2 j)
              for j = (if i (search end-delimiter string :start2 i))
                      then (if i (search end-delimiter string :start2 i))
              while (and i j)
          do (write-string (subseq string prev i) stream)
             (let ((instance (rest (assoc (subseq string (+ i start-len) j)
                                          values
                                          :test test))))
               (if instance
                (princ instance stream)
                (write-string (subseq string i (+ j end-len)) stream)))

          finally (write-string (subseq string prev) stream))))))
