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
(defreadtable :std
  (:merge :modern)
  (:dispatch-macro-char #\# #\" #'|#"-reader|)
  (:dispatch-macro-char #\# #\> #'|#>-reader|)
  #+cl-ppcre (:dispatch-macro-char #\# #\~ #'|#~-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\f #'|#f-reader|)
  (:dispatch-macro-char #\# #\$ #'|#$-reader|)))
