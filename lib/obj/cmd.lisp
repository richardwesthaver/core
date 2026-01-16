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
;; dynamically and are represented by simple symbols.

;; TODO:
;; - INTERACTIVE declaration
;; - DEFCOMMAND
;; - *COMMAND-TABLE*
;; - compile-command
;; - COMMAND type (class?)
;; - prefix-arg?
;; - READ-COMMAND/WRITE-COMMAND
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

(defvar *commands*)
(defvar *command-types*)
(defvar *command-table* (make-hash-table))

;; Set the interactive declaration information for this function. Each form in
;; ARGS corresponds with an element of the function's lambda list where the
;; CAR (or the form itself if an atom) is a COMMAND-TYPE designator.
(define-declaration interactive (spec env)
  (declare (ignore env))
  (values :declare (cons 'interactive spec)))

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

(defmacro define-command-type (name (input prompt) &body body)
  `(setf (command-type ,name)
         (lambda (,input ,prompt)
           ,@body)))

(defun call-interactively (command &optional (input ""))
  "Parse COMMAND's arguments from input according to its command spec then
execute it."
  (declare ((or string symbol) command)
           (string input)))

;; (defmethod call)

;; (defmethod exec ((self command)))

(defmacro with-commands (name &body body)
  "Eval BODY with *COMMANDS* bound to the value of (GETHASH NAME *COMMAND-TABLE*)."
  `(destructuring-bind (*commands* . *command-types*) (command-table ,name)
     ,@body))

(defun save-commands (name)
  "Set the value of NAME in *COMMAND-TABLE* using *COMMANDS* and *COMMAND-TYPES*."
  (setf (command-table name) (cons *commands* *command-types*)))

(defun copy-commands (name1 name2)
  (with-commands name1 (save-commands name2)))

(defmacro defcommand (name args &body body &environment env))
