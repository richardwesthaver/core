;;; cmd.lisp --- Generic Command Protocol

;; DEFCOMMAND

;;; Commentary:

;; This package provides a framework for defining COMMAND objects. Commands
;; are just ordinary functions or instances of a FUNCALLABLE-STANDARD-CLASS
;; equipped with the means to be called via a User Interface.

;; Similar to Emacs, we support an INTERACTIVE declaration which provides
;; information about how a user fills in the function arguments, often with
;; the use of dialogs and prompts. Unlike Emacs, we support a more lispy
;; specification than the prefix-strings and evaluated forms which Emacs
;; requires.

;; Instead of 'code letters' we support COMMAND-TYPEs which can be defined
;; dynamically and are represented by simple symbols. Command types are
;; functions which take a variable number of arguments which are passed to the
;; function from an interactive declaration.

;; TODO:
;; - DEFCOMMAND
;; - compile-command
;; - prefix-arg?
;; - READ-COMMAND/WRITE-COMMAND
;; - caching? should certainly cache command-types..
;; - explore posibilities with &aux extension..

#| emacs help
Code letters available are:
a -- Function name: symbol with a function definition.
b -- Name of existing buffer.
B -- Name of buffer, possibly nonexistent.
c -- Character (no input method is used).
C -- Command name: symbol with interactive function definition.
d -- Value of point as number.  Does not do I/O.
D -- Directory name.
e -- Parameterized event (i.e., one that’s a list) that invoked this command.
If used more than once, the Nth ‘e’ returns the Nth parameterized event.
This skips events that are integers or symbols.
f -- Existing file name.
F -- Possibly nonexistent file name.
G -- Possibly nonexistent file name, defaulting to just directory name.
i -- Ignored, i.e. always nil.  Does not do I/O.
k -- Key sequence (downcase the last event if needed to get a definition).
K -- Key sequence to be redefined (do not downcase the last event).
m -- Value of mark as number.  Does not do I/O.
M -- Any string.  Inherits the current input method.
n -- Number read using minibuffer.
N -- Numeric prefix arg, or if none, do like code ‘n’.
p -- Prefix arg converted to number.  Does not do I/O.
P -- Prefix arg in raw form.  Does not do I/O.
r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
R -- Active region: as ‘r’ but both nil unless ‘use-region-p’.  Does no I/O.
s -- Any string.  Does not inherit the current input method.
S -- Any symbol.
U -- Mouse up event discarded by a previous k or K argument.
v -- Variable name: symbol that is ‘custom-variable-p’.
x -- Lisp expression read but not evaluated.
X -- Lisp expression read and evaluated.
z -- Coding system.
Z -- Coding system, nil if no prefix arg.
|#

;;; Code:
(in-package :obj/cmd)

(defvar *commands* (make-hash-table))
(defvar *command-types* (make-hash-table))
(defvar *command-table* (make-hash-table))

;; Set the interactive declaration information for this function. Each form in
;; ARGS corresponds with an element of the function's lambda list where the
;; CAR (or the form itself if an atom) is a COMMAND-TYPE designator.
(define-declaration interactive (spec env)
  (declare (ignore env))
  (values :declare spec))

(defmacro %with-interactive (sym &body body &environment env)
  `(let ((,sym (declaration-information 'interactive ,env)))
     ,@body))

(defun command-table (name)
  (gethash name *command-table*))

(defun (setf command-table) (new name) (setf (gethash name *command-table*) new))

(defun command (name)
  (gethash name *commands*))

(defun (setf command) (new name) (setf (gethash name *commands*) new))

(defun command-type (name)
  (gethash name *command-types*))

(defun (setf command-type) (new name) (setf (gethash name *command-types*) new))

(defmacro define-command-type (name args &body body)
  "Define a new COMMAND-TYPE and store it in *COMMAND-TYPES*. ARGS is a
lambda-list which destructures the cdr of INTERACTIVE declaration forms.

Example:

(define-command-type :symbol (input prompt)
 (or (find-symbol
       (string-upcase
         (or (argument-pop input)
             ;; Whitespace messes up find-symbol.
             (string-trim \" \"
                          (completing-read (current-screen)
                                           prompt
                                           (let (acc)
                                             (do-symbols (s (find-package \"STD\"))
                                               (push (string-downcase (symbol-name s)) acc))
                                             acc)))
             (throw 'error \"Abort.\")))
       \"STD\")
     (throw 'error \"Symbol not in STD package\")))

(defcommand \"symbol\" (sym) 
 (declare (interactive (:symbol \"Pick a symbol: \")))
 (describe sym s))"
  `(setf (command-type ,name)
         (lambda ,args
           ,@body)))

(defun call-interactively (command &optional (input ""))
  "Parse COMMAND's arguments from input according to its command spec then
execute it."
  (declare ((or string symbol) command)
           (string input))
  (catch 'cmd
    (let* ((cmd (command command))
           (arglist (sb-introspect:function-lambda-list cmd))
          (in input))
      ;; TODO 2026-01-15: 
      (apply cmd in arglist))))

;; (defmethod call)

;; (defmethod exec ((self command)))

(defmacro with-commands (name &body body)
  "Eval BODY with *COMMANDS* bound to the value of (GETHASH NAME *COMMAND-TABLE*)."
  `(let ((*commands* (command-table ,name)))
     ,@body))

(defun save-commands (name)
  "Set the value of NAME in *COMMAND-TABLE* using *COMMANDS*."
  (setf (command-table name) *commands*))

(defun copy-commands (name1 name2)
  "Copy all commands and types from NAME1 to NAME2."
  (with-commands name1 (save-commands name2)))

;; future use
#+nil(defkernel command () ())

;; (defun command-info (name))

(defun compile-command (name &rest interactive)
  "Compile NAME as a COMMAND with the provided INTERACTIVE declaration
information. If NAME already designates a command it is re-compiled with the
new INTERACTIVE form in place.")

(defmacro defcommand (name args &body body &environment env)
  "Define a new COMMAND given NAME and ARGS which evaluates BODY. NAME may be
an atom which is added to *COMMANDS* or a list where the car is the name of
the *COMMAND-TABLE* entry to add this command to. ARGS is a typical lambda
list. A default command wrapper is provided in the case that BODY doesn't
include an INTERACTIVE declaration, else the DECLARATION-INFORMATION is
parsed from the environment and used to inform the wrapper.

INTERACTIVE declarations should match the lambda-list of ARGS with each form
being a COMMAND-TYPE or a cons where the car is a COMMAND-TYPE and the cdr is
the args to it."
  (multiple-value-bind (forms decl doc) (parse-body body :documentation t)
    `(setf (command ',name) (symbol-function (defun ,name ,args ,@(when doc `(,doc)) ,decl ,@forms)))))

;;; Init
(defmethod init ((self (eql :commands)) &key name)
  (setq *commands* (command-table name)))

(defmethod reset ((self (eql :commands)) &key name)
  (setq *commands* nil))
