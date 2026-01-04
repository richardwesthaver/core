;;; std/readtable.lisp --- The Standard Readtable

;; This readtable is accessible to systems which depend on the STD
;; package.

;;; Usage: (in-readtable :std)

;;; Code:
(IN-PACKAGE :STD/READTABLE)
(STD/NAMED-READTABLES:IN-READTABLE :STANDARD)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun |#`-reader| (stream sub-char numarg)
    "Sharp Backquote (#`) reader - quoted lambda shorthand.

Defines a lambda with the arg count determined by the numeric reader arg.

(funcall #2`(,a1 ,@a2) 0 '(1 2 3 4)) ;= (0 1 2 3 4)"
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                    collect (symb 'a i))
       ,(funcall
         (get-macro-character #\`) stream nil)))

  (defun |#l-reader| (stream sub num)
    "Sharp L reader - logical pathname translation."
    (declare (ignore sub num))
    `(translate-logical-pathname (pathname ,(read stream))))

  ;; Nestable suggestion from Daniel Herring
  (defun |#"-reader| (stream sub-char numarg)
    "Sharp Double-quote reader - nestable strings.

Output is quoted appropriated - simply wrap outer-most double-quotes in
sharps."
    (declare (ignore sub-char numarg))
    (let (chars (state 'normal) (depth 1))
      (loop do
               (let ((curr (read-char stream)))
                 (cond ((eq state 'normal)
                        (cond ((char= curr #\#)
                               (push #\# chars)
                               (setq state 'read-sharp))
                              ((char= curr #\")
                               (setq state 'read-quote))
                              (t
                               (push curr chars))))
                       ((eq state 'read-sharp)
                        (cond ((char= curr #\")
                               (push #\" chars)
                               (incf depth)
                               (setq state 'normal))
                              (t
                               (push curr chars)
                               (setq state 'normal))))
                       ((eq state 'read-quote)
                        (cond ((char= curr #\#)
                               (decf depth)
                               (if (zerop depth) (return))
                               (push #\" chars)
                               (push #\# chars)
                               (setq state 'normal))
                              (t
                               (push #\" chars)
                               (if (char= curr #\")
                                   (setq state 'read-quote)
                                   (progn
                                     (push curr chars)
                                     (setq state 'normal)))))))))
      (coerce (nreverse chars) 'string))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun segment-reader (stream ch n)
    "Recursively read a CH delimited sequence of strings from STREAM. N is a
recursion count. Used internally by the PPCRE reader (#~)."
    (if (> n 0)
        (let ((chars))
          (do ((curr (read-char stream)
                     (read-char stream)))
              ((char= ch curr))
            (push curr chars))
          (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (- n 1)))))))

(defmacro! scan-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

(defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
  ``(lambda (,',g!str)
      (ppcre:scan-to-strings
       ,(if (zerop (length ,g!mods))
            (car ,g!args)
            (format nil "(?~a)~a" ,g!mods (car ,g!args)))
       ,',g!str)))

(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#~-reader| (stream sub-char numarg)
    "Sharp-tilde reader - Perl-like Regexp shorthand.

NUMARG is the mode to use:
0 : scan-mode
1 : match-mode
2 : replace-mode

#1~/abc/ ;= #<function>
(funcall * \"123abc\") ;= \"abc\" #()

(funcall #2~/abc// \"abcdef\") ;= \"def\" T
(funcall #0~/abc/ \"abcdef\") ;= 0 3 #() #()"
    (declare (ignore sub-char))
    (ecase numarg
      (0 (scan-mode-ppcre-lambda-form
          (segment-reader 
           stream
           (read-char stream)
           1)))
      (1 (match-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          1)
          (coerce (loop for c = (read-char stream)
                        while (alpha-char-p c)
                        collect c
                        finally (unread-char c stream))
                  'string)))
      (2 (subst-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          2))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |{-reader| (stream inchar)
    "Curly-brace reader - curry shorthand.

The car of the 'curly-form' is a function which is curried with the cdr. The
cdr may contain the special symbol '_' which will be bound to the function and
indicates a recursive call (RCURRY instead of CURRY).

'{car _} ;= (THE (VALUES FUNCTION &OPTIONAL) (RCURRY #'CAR))

(funcall {car (list 1 2 3)}) ;= 1"
    (declare (ignore inchar))
    (let ((spec (read-delimited-list #\} stream t)))
      (if (typep (car spec) '(integer 0))
          ;; Number of missing arguments
          (let* ((n (pop spec))
                 (extra-args (loop repeat n collect (gensym "A"))))
            (if (eq (cadr spec) '_)
                (let ((provided-vars (loop repeat (length (cddr spec))
                                           collect (gensym "P"))))
                  `(let ,(mapcar #'list provided-vars (cddr spec))
                     (lambda ,extra-args (funcall (function ,(car spec))
                                                  ,@extra-args ,@provided-vars))))
                (let ((provided-vars (loop repeat (length (cdr spec))
                                           collect (gensym "P"))))
                  `(let ,(mapcar #'list provided-vars (cdr spec))
                     (lambda ,extra-args (funcall (function ,(car spec))
                                                  ,@provided-vars ,@extra-args))))))
          (if (eq (cadr spec) '_)
              `(the (values function &optional) (rcurry (function ,(car spec)) ,@(cddr spec)))
              `(the (values function &optional) (curry (function ,(car spec)) ,@(cdr spec)))))))

  (defun |[-reader| (stream inchar)
    "Square-bracket reader - compose shorthand.

'[#'car #'cdr] ;= (THE (VALUES FUNCTION &OPTIONAL) (COMPOSE #'CAR #'CDR))

(funcall ['car 'cdr] (list 1 2 3)) ;= 2"
    (declare (ignore inchar))
    (list 'the '(values function &optional)
          (cons 'compose (read-delimited-list #\] stream t)))))

;; f-strings
;; ref: https://realpython.com/python-f-strings/
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO 2025-06-23: 
  (defun |#f-reader| (stream subchar num)
    "Sharp-f reader - Python-like f-strings.

#f\"foo: {foo}, bar: {bar}~%\" ;= (format nil \"foo: ~A, bar: ~A~%\" foo bar)"
    (declare (ignore subchar))
    (format (case num
              (1 t)
              (2 *debug-io*)
              (t nil))
            (read stream))))

;; Define the standard readtable with built-in functionality. We overwrite the
;; braces [] and {} but ! and ? are free for now.
(defreadtable :std
  "The standard readtable, available for use internally in core source code or
externally by users. Don't modify this readtable directly - create your own
copy if necessary."
  (:merge :standard)
  ;; curry
  (:macro-char #\{ #'|{-reader|)
  (:macro-char #\} (get-macro-character #\) ))
  (:macro-char #\[ #'|[-reader|)
  (:macro-char #\] (get-macro-character #\) ))
  ;; strings
  (:dispatch-macro-char #\# #\" #'|#"-reader|)
  (:dispatch-macro-char #\# #\f #'|#f-reader|)
  ;; regex
  (:dispatch-macro-char #\# #\~ #'|#~-reader|)
  ;; lambdas
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  ;; logical paths
  (:dispatch-macro-char #\# #\l #'|#l-reader|))
