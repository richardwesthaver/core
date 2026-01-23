;;; tui.lisp --- Terminal User Interface

;; Utilities for building Terminal UIs.

;;; Commentary:

;; This package provides 

;;; Code:
(in-package :cli/tui)

(define-command-type secret (input &optional (prompt "Secret: "))
  (format *query-io* prompt)
  (force-output *query-io*)
  (let ((val (without-echo (read-arg input))))
    (conceal val)))

(defun completing-read (prompt completions &optional (default :error))
  "Print PROMPT then read input using linedit with completions. DEFAULT is the
value returned when the input doesn't match a member of COMPLETIONS. A default
value of :ERROR indicates that an error will be signaled when input doesn't
match."
  (let ((val (linedit:linedit :prompt prompt :completions completions)))
    (cond 
      ((member val completions :test 'equal) val)
      ((eql default :error) (error 'invalid-argument :reason (format nil "input must match one of: ~S" completions) :item val))
      ((not (sequence:emptyp val)) val)
      (t default))))

(defun completing-read-form (prompt completions &key (default :error) (prompt2 "> ") (package *package*) (test 'equalp))
  "Print PROMPT then read input using formedit with completions. DEFAULT is the
value returned when the input doesn't match a member of COMPLETIONS. A default
value of :ERROR indicates that an error will be signaled when input doesn't
match. The note-worthy difference between this function and COMPLETING-READ is
that the input is passed to READ-FROM-STRING."
  (let ((val (with-safe-io-syntax (package) 
               (read-from-string (linedit:formedit :prompt1 prompt :prompt2 prompt2 :completions completions)))))
    (cond 
      ((member val completions :test test) val)
      ((eql default :error)
       (error 'invalid-argument :reason (format nil "input must match one of: ~S" completions) :item val))
      (val val)
      (t default))))

(define-command-type cmd (input &optional (prompt "Command: ") completions (default :error))
  (let ((*query-io* input))
    (completing-read prompt (or completions (mapcar (lambda (x) (string-downcase (car x))) (commands))) default)))
