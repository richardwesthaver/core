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

(deftype %shell-state () '(member :sh :dolla :pound))

(defun plain-shell-reader (stream)
  (let (out (state :sh))
    (declare (type %shell-state state))
    (loop for c = (read-char stream)
          do (cond
               ((eq state :sh)
                (case c
                  (#\$ (setq state :dolla))
                  (#\# (setq state :pound))
                  (t (push c out))))
               ((eq state :pound)
                (if (char= c #\,)
                    ;; slow
                    (push (coerce (format nil "~A" (eval (read stream nil nil))) 'list) out)
                    (progn 
                      (push #\# out)
                      (push c out)))
                (setq state :sh))
               ((eq state :dolla)
                (if (char= c #\#)
                    (return)
                    (progn
                      (setq state :sh)
                      (push #\$ out)
                      (push c out))))))
    (concatenate 'string
                 (flatten (nreverse out)))))

(defmacro define-process-output-handler (type &body body)
  "Define a new function which handles the result of a SB-EXT:PROCESS in
the context of the $#-reader macro."
  (declare (ignore type body)))

(defun |#$-reader| (stream sub-char numarg)
  "Switch on the shell reader, parsing STREAM and returning a
shell program or executing it. In other words, this is an
implementation of the lazy version of SHCL's #$-reader.

Similar to shcl, we add some reader extensions to enable embedding
lisp forms and other goodies.

#0$ x=#,(* 2 2) 
echo $x
$#
;; => 4

KLUDGE: an escaped SYMBOL can't be immediately followed by the closing tag '$#' - this causes the reader to consume those characters as part of the symbol name. One thing we might end up doing is checking for those characters in the input and unreading those 2 chars.

An escaped form with parens like the following works fine:

#0$echo #,(+ 2 2)$# ;; => 4"
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
          (lambda (&key (output *standard-output*) (wait t) (status-hook))
            (case output
              (:string (string-right-trim
                        '(#\Newline)
                        (with-output-to-string (s)
                          (sb-ext:run-program *shell* args
                                              :directory directory
                                              :output s
                                              :input *shell-input*
                                              :wait wait
                                              :status-hook status-hook))))
              (:integer (parse-integer
                         (string-right-trim
                          '(#\Newline)
                          (with-output-to-string (s)
                            (sb-ext:run-program *shell* args
                                                :directory directory
                                                :output s
                                                :input *shell-input*
                                                :wait wait
                                                :status-hook status-hook)))))
              (t (sb-ext:run-program *shell*
                                     args
                                     :directory directory
                                     :output output
                                     :input *shell-input*
                                     :wait wait
                                     :status-hook status-hook))))))))

(defreadtable :shell
  "The shell readtable"
  (:merge :std)
  (:dispatch-macro-char #\# #\$ #'|#$-reader|))
