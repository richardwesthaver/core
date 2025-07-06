;;; tui.lisp --- Terminal User Interface

;; Utilities for building Terminal UIs.

;;; Commentary:

;; This package provides 

;;; Code:
(in-package :cli/tui)

(defvar *prompt2* "> ")

(defun completing-read (prompt completions &optional default)
  "Print PROMPT then read input using linedit with completions. DEFAULT is the
value returned when the input doesn't match a member of COMPLETIONS. A default
value of :ERROR indicates that an error will be signaled when input doesn't
match."
  (let ((val (linedit:formedit :prompt1 prompt :prompt2 *prompt2* :completions completions)))
    (if default
        (if (member val completions :test 'equal)
            val
            (if (eql default :error)
                (error 'invalid-argument :reason (format nil "input must match one of: ~S" completions) :item val)
                val))
        val)))

;; (defmacro defprompt (name ))
