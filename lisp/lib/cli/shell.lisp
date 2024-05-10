;;; lib/cli/shell.lisp --- shell utils

;; utils for working with shells in different environments

;;; Commentary:

;;; #$ Read Macro

;; A read macro is accessible in the named readtable :SHELL. It has
;; three modes of operation: read, compile, and eval. In read mode,
;; input is parsed and embedded lisp forms are expanded. The string is
;; returned as is. In eval mode, embedded lisp forms are expanded and
;; the resulting string is wrapped in a call to
;; SB-EXT:RUN-PROGRAM. Finally, in eval mode the compiled function is
;; called with default arguments and the result of that call is
;; returned.

;;; Code:
(in-package :cli/shell)
(in-readtable :std)

(defparameter *shell* "/bin/bash")
(defparameter *shell-directory* nil)
(defparameter *shell-input* nil)

(defun plain-shell-reader (stream)
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

;; (defun lisp-shell-reader (stream numarg))

(defmacro define-process-output-handler (type &body body)
  "Define a new function which handles the result of a SB-EXT:PROCESS in
the context of the $#-reader macro.")

(defun |#/-reader| (stream sub-char numarg)
  "parse STREAM using the LISP-SHELL-READER, expanding 'unquoted' lisp
forms and injecting them back in the string."
  (declare (ignore sub-char))
  (lisp-shell-reader stream numarg))

(defun |#$-reader| (stream sub-char numarg)
  "Switch on the shell reader, parsing STREAM and returning a
shell program or executing it. In other words, this is an
implementation of the lazy version of SHCL's #$-reader.

Similar to shcl, we add some reader extensions to enable embedding
lisp forms and other goodies.

#0$ x=,(* 2 2) 
echo $x
$#
;; => 4"
  (declare (ignore sub-char) ((or (integer 0 9) null) numarg))
  (let ((str (plain-shell-reader stream)))
    (if numarg
        (progn
          (cond
            ((= numarg 0)
             (string-right-trim '(#\Newline)
                                (with-output-to-string (s)
                                  (sb-ext:run-program *shell*
                                                      (list "-c" (format nil "~a" str))
                                                      :directory (or *shell-directory* *default-pathname-defaults*)
                                                      :output s
                                                      :input *shell-input*))))
            (t (nyi!))))
        (let ((args (list "-c" (format nil "~a" str)))
              (directory (or *shell-directory* *default-pathname-defaults*)))
          (lambda (&key (output *standard-output*) (wait t))
            (case output
              (:string (string-right-trim
                        '(#\Newline)
                        (with-output-to-string (s)
                          (sb-ext:run-program *shell* args
                                              :directory directory
                                              :output s
                                              :input *shell-input*
                                              :wait wait))))
              (:integer (parse-integer
                         (string-right-trim
                          '(#\Newline)
                          (with-output-to-string (s)
                            (sb-ext:run-program *shell* args
                                                :directory directory
                                                :output s
                                                :input *shell-input*
                                                :wait wait)))))
              (t (sb-ext:run-program *shell*
                                     args
                                     :directory directory
                                     :output output
                                     :input *shell-input*
                                     :wait wait))))))))

(defreadtable :shell
  "The shell readtable"
  (:merge :std)
  (:dispatch-macro-char #\# #\$ #'|#$-reader|)
  (:dispatch-macro-char #\# #\/ #'|#/-reader|))
