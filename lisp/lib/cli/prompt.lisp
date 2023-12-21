;;; lib/cli/prompt.lisp --- Basic CLI Prompts

;; the interface needs some work to work correctly with SBCL streams

;;; Code:
(in-package :cli/prompt)

(declaim (inline completing-read))
(defun completing-read (prompt collection
			&key (history nil) (default nil)
			  (key nil) (test nil) 
                          (input *standard-input*)
                          (output *standard-output*))

  "A simplified COMPLETING-READ for common-lisp.

The Emacs completion framework includes a function called
`completing-read' which prompts the user for input from the
mini-buffer. It is a very flexible interface which can be used to read
user input programatically. This is incredibly useful for building
data entry interfaces -- for example see the `defprompt' macro.

Obviously writing a completion framework is out-of-scope, but we can
simulate one by embedding a DSL in our prompters if we choose. For
example, perhaps we treat a single '?' character as a request from the
user to list valid options while continue waiting for input."
  (princ prompt output)
  ;; ensure we empty internal buffer
  (finish-output output)
  (let* ((coll collection)
         (res (let ((%i (read-line input)))
                (if (> (length %i) 0)
                    %i default)))
	 (r (if coll
		(find res coll :key key :test test)
		res)))
    (prog1
	r
      (setf history (push r history)))))

(defmacro defprompt (var &optional prompt)
  "Generate a 'prompter' from list or variable VAR and optional
PROMPT string.

This isn't an ideal solution as it does in fact expose a dynamic
variable (VAR-prompt-history). We should generate accessors and
keep the variables within lexical scope of the generated
closure."
  (with-gensyms (s p h)
    `(let ((,s (if (boundp ',var) (symbol-value ',var) 
		   (progn 
		     (defvar ,(symbolicate var) nil)
		     ',(symbolicate var))))
           (,p (when (stringp ,prompt) ,prompt)) ;; prompt string
           (,h ',(symbolicate var '-prompt-history))) ;; history symbol
       (defvar ,(symbolicate var '-prompt-history) nil)
       (defun ,(symbolicate var '-prompt) ()
	 ,(format nil "Prompt for a value from `~A', use DEFAULT if non-nil
and no value is provided by user, otherwise fallback to the `car'
of `~A-PROMPT-HISTORY'." var var)
	 (completing-read
          (format nil "~A [~A]: "
		  (or ,p ">")
		  (car (symbol-value ,h)))
	  ,s :history ,h :default nil)))))
