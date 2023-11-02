;;; std/util.el --- standard utils  -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)

(defun group (source n)
  "This is Paul Graham's group utility from 'On Lisp'.

Group a list of arguments SOURCE by any provided grouping amount
N.

For example:
(group '(foo 2 bar 4) 2) ;=> ((foo 2) (bar 4))
(group '(a b c d e f) 3) ;=> ((a b c) (d e f))
"
  (when (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons
                                    (cl-subseq source 0 n)
                                    acc))
                       (nreverse
                        (cons source acc))))))
    (when source (rec source nil))))

(defun flatten (x)
  "Paul Graham's flatten utility from 'On Lisp'.

Given a tree X, return all the 'leaves' of the tree."
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec
                             (car x)
                             (rec (cdr x) acc))))))
    (rec x nil)))

(defun mkstr (&rest args)
  "Paul Graham's mkstr utility from 'On Lisp'.

Coerce ARGS into a single string and return it."
  (let* ((s ""))
    (dolist (a args)
      (cond
       ((null a) nil)
       ((sequencep a) (setq s (concat s a)))
       ((numberp a) (setq s(concat s (number-to-string a))))
       ((symbolp a) (setq s(concat s (symbol-name a))))))
    s))

(defun symb (&rest args)
  "Paul Graham's symb utility from 'On Lisp'.

Concat ARGS and return a newly interned symbol."
  (intern (apply #'mkstr args)))

(defmacro when-sys= (name body)
  "(when (string= (system-name) NAME) BODY)"
  `(when ,(string= (system-name) name) ,body))

(defun add-to-load-path (&rest paths)
  "Add PATHS to `load-path'."
  (mapc (lambda (x)
          (cond
           ((listp x) (mapc #'add-to-load-path x))
           ('_ (cl-pushnew x load-path))))
        paths))

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path."
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(provide 'util)
