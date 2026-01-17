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
;; - command environment?
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

;;; Variables
(defvar *command* nil)
(defvar *default-command-class* 'command)
(defvar *commands* (make-hash-table))
(defvar *command-types* (make-hash-table))
(defvar *command-table* (make-hash-table))
(defvar *command-delimiter* #\;
  "A character used to indicate the start of a new command within the same line.")

;;; Conditions
(define-condition command-condition () ())
(define-condition command-error (command-condition)
  ((name :initarg :name :reader error-name)))
(define-condition undefined-command (command-error)
  ((args :initarg :args :reader error-args :initform nil))
  (:report (lambda (c s) 
             (format s "Undefined command ~A~@[ with args ~{~S~^ ~}~]" 
                     (error-name c) (error-args c)))))

(define-condition undefined-command-type (command-error)
  ((args :initarg :args :reader error-args :initform nil))
  (:report (lambda (c s) 
             (format s "Undefined command type: ~A~@[ with args ~{~S~^ ~}~]" 
                     (error-name c) (error-args c)))))

(define-condition invalid-itype (command-error syntax-error) 
  ((args :initarg :args :reader error-args :initform nil))
  (:default-initargs 
   :ast (declaration-information 'interactive))
  (:report (lambda (c s) (format s "Invalid INTERACTIVE declaration: ~S~@[ given args ~{~S~^ ~}~]" (ast c) (error-args c)))))

(definline undefined-command (name &optional args) (error 'undefined-command :name name :args args))
(definline undefined-command-type (name &optional args) (error 'undefined-command-type :name name :args args))
(definline invalid-itype (&optional form) (error 'invalid-itype :ast form))

;;; Declarations
;; Set the interactive declaration information for this function. Each form in
;; ARGS corresponds with an element of the function's lambda list where the
;; CAR (or the form itself if an atom) is a COMMAND-TYPE designator.
(defun check-itype (form &optional lambda-list)
  "Validate the INTERACTIVE typespec FORM, optionally against a COMMAND's
LAMBDA-LIST."
  (assert (listp form) nil 'invalid-itype :ast form :args lambda-list)
  (when (and lambda-list form)
    (assert (match-lambda-lists form lambda-list) nil 'invalid-itype :ast form :args lambda-list)))

(define-declaration interactive (spec env)
  (declare (ignore env))
  (values :declare spec))

(defmacro %with-interactive (sym &body body &environment env)
  `(let ((,sym (declaration-information 'interactive ,env)))
     ,@body))

#+nil
(locally (declare (interactive (file "file: ") (symbol "symbol: ")))
  (%with-interactive i i))
#+nil
(defun foo (a b)
  (declare (interactive (file "file: ") (symbol "symbol: ")))
  (values a b (%with-interactive i i)))

;;; Accessors
(defun make-commands (name &optional (commands (make-hash-table)) (types (make-hash-table)))
  (setf (command-table name) (cons commands types)))

(defun command-table (name)
  (gethash name *command-table*))

(defun (setf command-table) (new name) (setf (gethash name *command-table*) new))

(defun command (name &optional (commands *commands*))
  (gethash name commands))

(defun (setf command) (new name &optional (commands *commands*)) (setf (gethash name commands) new))

(defun command-type (name &optional (types *command-types*))
  (gethash name types))

(defun (setf command-type) (new name &optional (command-types *command-types*)) 
  (setf (gethash name command-types) new))

(defun commands (&optional name)
  (hash-table-alist (if name (car (command-table name)) *commands*)))

(defun command-types (&optional name)
  (hash-table-alist (if name (cdr (command-table name)) *command-types*)))

;;; Command Types
(defmacro define-command-type (name args &body body)
  "Define a new COMMAND-TYPE and store it in *COMMAND-TYPES* if NAME is an atom,
else the car of NAME designates the value of *COMMAND-TABLE* to modify. ARGS
is a lambda-list which destructures the cdr of INTERACTIVE argtype forms.

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
  `(setf ,(if (atom name) 
              `(command-type ',name)
              `(command-type ',(second name) (cdr (or (command-table ',(car name)) (make-commands ',(car name))))))
         (lambda ,args
           ,@body)))

;; IO
(deffmt fmt-command "~(~A~)~@[ ~{~S~^ ~}~]" "Format a COMMAND string given a name and list of args.")

(defun read-command (&optional (stream *standard-input*))
  (with-input-from-string (s (read-line stream))
    (let* ((form (read-lisp-until-end s))
           (cmd (or (command (car form)) (undefined-command (car form) (cdr form))))
           (ll (function-lambda-list cmd)))
      (values form cmd ll))))

(defun write-command (cmd &optional args (stream *standard-output*))
  (fmt-command stream cmd args)
  (fresh-line stream))

(defun parse-command (str)
  "Parse a COMMAND from STR."
  (with-input-from-string (s str)
    (read-command s)))

(defun call-interactively (command &optional input)
  "Parse COMMAND's arguments from input according to its command spec then
execute it."
  (declare ((or string symbol) command)
           ((or null string) input))
  (catch 'cmd
    (let* ((cmd (or (command command) (undefined-command command)))
           (ll (function-lambda-list cmd))
           (in input))
      ;; TODO 2026-01-15:
      (if ll
          (apply cmd in ll)
          (progn
            (when input (warn "too many args to command ~A from input: ~S" command input))
            (funcall command))))))

;; (defmethod call)

;; (defmethod exec ((self command)))

(defmacro with-commands (name &body body)
  "Eval BODY with (*COMMANDS* . *COMMAND-TYPES*) bound to the value of (GETHASH
NAME *COMMAND-TABLE*)."
  `(destructuring-bind (*commands* . *command-types*) (command-table ,name)
     ,@body))

(defun save-commands (name)
  "Set the value of NAME in *COMMAND-TABLE* using *COMMANDS*."
  (setf (command-table name) (cons *commands* *command-types*)))

(defun copy-commands (name1 name2)
  "Copy all commands and types from NAME1 to NAME2."
  (with-commands name1 (save-commands name2)))

;; future use
(deftype interactive-function () `function)

(defkernel command ()
  ((interactive :initarg :interactive :initform nil :reader interactive))
  (:documentation "Commands are INTERACTIVE-FUNCTIONs or instances of this class.

The INTERACTIVE declaration corresponds to the slot of the same name in this
class - both may be used to specify the ITYPE (interaction typespec) of the
command."))

(defmethod initialize-instance :after ((self command) &key kernel)
  (when kernel
    (setf (kernel self) kernel)))

(definline commandp (cmd) 
  (declare (values boolean))
  (and (or (typep cmd 'command)
           (command cmd))
       t))

;; (defun command-info (name))

#+nil
(defun compile-command (name command &optional env)
  "Compile COMMAND as a FUNCTION given ENV with optional INTERACTIVE declaration
information."
  (compile name (sb-cltl2:enclose (kernel-expression command) env)))

(defmacro defcommand (name args &body body)
  "Define a new COMMAND given NAME and ARGS which evaluates BODY. NAME may be
an atom which is added to *COMMANDS* or a list where the car is the name of
the *COMMAND-TABLE* entry to add this command to. ARGS is a typical lambda
list. A default command wrapper is provided in the case that BODY doesn't
include an INTERACTIVE declaration, else the ITYPE is parsed and used to
inform the wrapper.

INTERACTIVE declarations should match the lambda-list of ARGS with each form
being a COMMAND-TYPE or a cons where the car is a COMMAND-TYPE and the cdr contains
the args to it."
  (let ((%cmd* (if (atom name) `(command ',name) 
                   `(command ',(second name) (car (or (command-table ',(car name)) (make-commands ',(car name))))))))
    (multiple-value-bind (forms decl doc) (parse-body body :documentation t)
      (let ((%int (when decl (cdr (assoc 'interactive (cdar decl)))))) ;; interactive typespec
        (check-itype %int args) ; validate
        (with-gensyms (%cmd)
          `(let ((,%cmd (make-instance *default-command-class* 
                          :interactive 
                          ',(collecting
                              (mapc
                               (lambda (x)
                                 (unless (member x lambda-list-keywords)
                                   (let ((name x)
                                         (args)) 
                                     (unless (atom x)
                                       (setf name (car x)
                                             args (cdr x)))
                                     (collect `(funcall (command-type ,name) ,@args)))))
                               %int)))))
             (setf (kernel ,%cmd) ; set the kernel slot of this COMMAND instance
                   ;; currently we compile a function in the current package
                   ;; with the same name - not strictly necessary and prone to
                   ;; name conflicts (user beware).
                   (lambda ,args                     
                     ,@decl
                     ,@forms)
                   ,@(when doc `((kernel-documentation ,%cmd) ,doc))
                   ,%cmd* ,%cmd)))))))

#+nil
(progn
  (define-command-type :test ())
  (defcommand art (a b &optional c) (declare (interactive :test :test &optional :test)) (values a b c)))

;;; Init
(defmethod init ((self (eql :commands)) &key name)
  (if name
      (let ((cons (command-table name)))
        (if cons
            (setq *commands* (car cons)
                  *command-types* (cdr cons))
            (make-commands name)))
      (reset :commands)))

(defmethod reset ((self (eql :commands)) &key name)
  (if name (remhash name *command-table*)
      (setq *commands* (make-hash-table)
            *command-types* (make-hash-table))))
