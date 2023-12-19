;;; std/readtable.lisp --- The Standard Readtable

;; This readtable is accessible to systems which depend on the STD
;; package.

;;; Usage: (in-readtable :std)

;;; Code:
(in-package :std)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                    collect (symb 'a i))
       ,(funcall
         (get-macro-character #\`) stream nil)))

  (defun |#f-reader| (stream sub-char numarg)
    (declare (ignore stream sub-char))
    (setq numarg (or numarg 3))
    (unless (<= numarg 3)
      (error "Bad value for #f: ~a" numarg))
    `(declare (optimize (speed ,numarg)
                        (safety ,(- 3 numarg)))))

  (defun |#$-reader| (stream sub-char numarg)
    "Switch on the shell reader, parsing STREAM and returning a
POSIX-compliant shell program as a string. In other words, this is an
implementation of the lazy version of SHCL's #$-reader.

Similar to shcl, we add some reader extensions to enable embedding
lisp forms and other goodies.

#$ x=,(* 2 2) 
echo $x
$#
;; => 4"
    (declare (ignore sub-char numarg))
    (let (chars (state 'sh))
      (loop do
	(let ((c (read-char stream)))
	  (cond 
	    ((eq state 'sh)
	     (when (char= c #\$) (setq state 'dolla))
	     (push c chars))
	    ((eq state 'dolla)
	     (cond
	       ((char= c #\#)
		;; remove trailing '$'
		(pop chars)
		(return))
	       (t (setq state 'sh) (push c chars)))))))
      (coerce (nreverse chars) 'string))))

;; Nestable suggestion from Daniel Herring
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#"-reader| (stream sub-char numarg)
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

                                        ; This version is from Martin Dirichs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#>-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= #\newline curr))
        (push curr chars))
      (let ((pattern (nreverse chars))
            output)
        (labels ((match (pos chars)
                   (if (null chars)
                       pos
                       (if (char= (nth pos pattern) (car chars))
                           (match (1+ pos) (cdr chars))
                           (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
          (do (curr
               (pos 0))
              ((= pos (length pattern)))
            (setf curr (read-char stream)
                  pos (match pos (list curr)))
            (push curr output))
          (coerce
           (nreverse
            (nthcdr (length pattern) output))
           'string))))))

                                        ; (set-dispatch-macro-character #\# #\> #'|#>-reader|)

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
  ``(lambda (,',g!str)
      (cl-ppcre:scan-to-strings
       ,(if (zerop (length ,g!mods))
            (car ,g!args)
            (format nil "(?~a)~a" ,g!mods (car ,g!args)))
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

#+cl-ppcre
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#~-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let ((mode-char (read-char stream)))
      (cond
        ((char= mode-char #\m)
         (match-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          1)
          (coerce (loop for c = (read-char stream)
                        while (alpha-char-p c)
                        collect c
                        finally (unread-char c stream))
                  'string)))
        ((char= mode-char #\s)
         (subst-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          2)))
        (t (error "Unknown #~~ mode character"))))))

;; #+cl-ppcre (set-dispatch-macro-character #\# #\~ #'|#~-reader|)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lcurly-brace-reader (stream inchar)
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

  (defun lsquare-brace-reader (stream inchar)
    (declare (ignore inchar))
    (list 'the '(values function &optional)
          (cons 'compose (read-delimited-list #\] stream t))))

  (defun langle-quotation-reader (stream inchar)
    (declare (ignore inchar))
    (let ((contents (read-delimited-list #\» stream t))
          (args (gensym "langle-quotation-reader")))
      `(lambda (&rest ,args)
         (,(car contents)                 ; Join function (or macro).
          ,@(mapcar (lambda (fun) `(apply ,fun ,args)) (cdr contents))))))

  (defun lsingle-pointing-angle-quotation-mark-reader (stream inchar)
    (declare (ignore inchar))
    (flet ((function-p (form) (functionp (ignore-errors (eval form)))))
      (let ((contents (read-delimited-list #\› stream t))
            (arg (gensym "lsingle-pointing-angle-quotation-mark-reader")))
        `(lambda (,arg)
           (,(car contents)               ; Case form.
            ,@(case (car contents)       ; If/when/unless guard.
                ((if when unless)
                 `((funcall ,(cadr contents) ,arg)))
                (cond nil)
                (t (list arg)))
            ,@(if (member (car contents) '(if when unless)) ; Clauses.
                  (mapcar (lambda (clause)
                            (if (function-p clause)
                                `(funcall ,clause ,arg)
                                clause))
                          (cddr contents))
                  (mapcar (lambda (clause)
                            `(,(if (function-p (car clause))
                                   `(funcall ,(car clause) ,arg)
                                   (car clause))
                              ,(if (function-p (cadr clause))
                                   `(funcall ,(cadr clause) ,arg)
                                   (cadr clause))))
                          (cdr contents)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defreadtable :std
    (:merge :modern)
    ;; curry
    (:macro-char #\{ #'std::lcurly-brace-reader)
    (:macro-char #\} (get-macro-character #\) ))
    (:macro-char #\[ #'std::lsquare-brace-reader)
    (:macro-char #\] (get-macro-character #\) ))
    (:macro-char #\« #'std::langle-quotation-reader)
    (:macro-char #\» (get-macro-character #\) ))
    (:macro-char #\‹ #'std::lsingle-pointing-angle-quotation-mark-reader)
    (:macro-char #\› (get-macro-character #\) ))
    ;; strings
    (:dispatch-macro-char #\# #\" #'|#"-reader|)
    (:dispatch-macro-char #\# #\> #'|#>-reader|)
    ;; regex
    (:dispatch-macro-char #\# #\~ #'|#~-reader|)
    ;; lambdas
    (:dispatch-macro-char #\# #\` #'|#`-reader|)
    (:dispatch-macro-char #\# #\f #'|#f-reader|)
    ;; shell
    (:dispatch-macro-char #\# #\$ #'|#$-reader|)))
