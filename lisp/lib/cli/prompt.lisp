;;; lib/cli/prompt.lisp --- Basic CLI Prompts

;; TODO

;;; Code:
(in-package :cli/prompt)
(declaim (optimize (speed 3) (debug 1)))

(defvar *completion-trigger* #\?)

(defun completing-read (prompt collection
			&key (history nil) (default nil)
                             (require-match t)
			     (test #'equalp) 
                             (input *query-io*)
                             (output *query-io*)
                             (reader #'read-line)
                             (hook))
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
  (declare (list collection)
           (function test reader hook)
           (boolean require-match)
           (stream input output))
  (labels ((print-coll ()
             (println collection output))
           (ask ()
             (princ prompt output)
             (finish-output output)
             (listen input)
             (let ((line (funcall reader input)))
               (if (equiv *completion-trigger* line)
                   (progn
                     (print-coll)
                     (ask))
                   (etypecase line
                     (string (if (> (length line) 0)
                                 line
                                 default))
                     (character (if (char= line #\Newline)
                                    default
                                    line))
                     (null default)
                     (t line))))))
    (let ((res (ask)))
      (when history (push res history))
      (when (and collection require-match)
          (setf res (find res collection :test test)))
      (when hook
        (setf res (funcall hook res)))
      res)))

(defmacro defprompt (var &key (prompt ">") collection default input output reader test hook)
  "Generate a 'prompter' from list or variable VAR and optional
PROMPT string.

This isn't an ideal solution as it does in fact expose a dynamic
variable (VAR-prompt-history). We should generate accessors and
keep the variables within lexical scope of the generated
closure."
  (with-gensyms (h)
    `(let ((,h ',(symbolicate '* var '-prompt-history*))) ;; history symbol
       (defvar ,(symbolicate '* var '-prompt-history*) nil)
       (defun ,(symbolicate var '-prompt) ()
	 ,(format nil "Prompt for a value from `~A', use DEFAULT if non-nil
and no value is provided by user, otherwise fallback to the `car'
of `~A-PROMPT-HISTORY'." var var)
	 (completing-read
          (format nil "~A [~@[~A~]]: "
                  ,prompt
		  (or ,default (car (symbol-value ,h))))
	  ,collection
          :history ,h
          :default ,default
          ,@(when input (list :input input))
          ,@(when output (list :output output))
          ,@(when reader (list :reader reader))
          ,@(when test (list :test test))
          ,@(when hook (list :hook hook)))))))

