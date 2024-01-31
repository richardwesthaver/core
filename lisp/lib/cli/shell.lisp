;;; lib/cli/shell.lisp --- shell utils

;; utils for working with shells in different environments

;;; Commentary:

;;; Code:
(in-package :cli/shell)
(in-readtable :std)

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
      (coerce (nreverse chars) 'string)))

(defreadtable :shell
  (:merge :std)
  (:dispatch-macro-char #\# #\$ #'|#$-reader|))
