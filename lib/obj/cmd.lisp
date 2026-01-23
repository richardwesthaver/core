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
(defvar *command*)
(defvar *command-class* 'command)
(defvar *commands* (make-hash-table))
(defvar *command-types* (make-hash-table))
(defvar *command-table* (make-hash-table))
(defvar *command-delimiter* #\;
  "A character used to indicate the start of a new command within the same line.")
(defparameter *command-names-p* nil
  "Indicates whether commands will be defined with LAMBDA (NIL) or DEFUN (T).")
(defhook *command-hook* ((:pre) (:post) (:eval)))

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

(define-condition invalid-command-type (command-error syntax-error)
  ((args :initarg :args :reader error-args :initform nil))
  (:report (lambda (c s) 
             (format s "Invalid command-type ~A given args: ~S" 
                     (error-name c) (error-args c)))))

(define-condition invalid-itype (command-error syntax-error) 
  ((args :initarg :args :reader error-args :initform nil))
  (:default-initargs 
   :ast (declaration-information 'interactive))
  (:report (lambda (c s) (format s "Invalid INTERACTIVE declaration: ~S~@[ given args ~{~S~^ ~}~]" (ast c) (error-args c)))))

(definline undefined-command (name &optional args) (error 'undefined-command :name name :args args))
(definline undefined-command-type (name &optional args) (error 'undefined-command-type :name name :args args))
(definline invalid-command-type (name &optional args) (error 'invalid-command-type :name name :args args))
(definline invalid-itype (&optional form) (error 'invalid-itype :ast form))

;;; Interactive
(deftype itype () '(list))

(defun check-itype (form &optional lambda-list)
  "Validate the INTERACTIVE typespec FORM, optionally against a COMMAND's
LAMBDA-LIST."
  (assert (listp form) nil 'invalid-itype :ast form :args lambda-list)
  (when (and lambda-list form)
    (assert (match-lambda-lists (mapcar (lambda (x) (if (consp x) (car x) x)) form) lambda-list) nil 'invalid-itype :ast form :args lambda-list)))

;; Set the interactive declaration information for this function. Each form in
;; ARGS corresponds with an element of the function's lambda list where the
;; CAR (or the form itself if an atom) is a COMMAND-TYPE designator.
(define-declaration interactive (spec env)
  (declare (ignore env))
  (values :declare spec))

#+nil
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
(definline cmd-intern (name) (keywordicate (string-upcase name)))

(defun make-commands (name &optional (commands (make-hash-table)) (types (make-hash-table)))
  (setf (command-table name) (cons commands types)))

(defun command-table (name)
  (gethash (cmd-intern name) *command-table*))

(defun (setf command-table) (new name) (setf (gethash (cmd-intern name) *command-table*) new))

(defun command (name &optional (commands *commands*))
  (gethash (cmd-intern name) commands))

(defun (setf command) (new name &optional (commands *commands*)) 
  (setf (gethash (cmd-intern name) commands) new))

(definline command-alias (new old)
  (setf (command new) (command old)))

(defun command-type (name &optional (types *command-types*))
  (gethash (cmd-intern name) types))

(defun (setf command-type) (new name &optional (command-types *command-types*)) 
  (setf (gethash (cmd-intern name) command-types) new))

(defun commands (&optional name)
  (hash-table-alist (if name (car (command-table name)) *commands*)))

(defun command-types (&optional name)
  (hash-table-alist (if name (cdr (command-table name)) *command-types*)))

;;; Command Types
(defmacro define-command-type (name args &body body)
  "Define a new COMMAND-TYPE and store it in *COMMAND-TYPES* if NAME is an atom,
else the car of NAME designates the value of *COMMAND-TABLE* to modify. ARGS
is a lambda-list where the car is a required argument designating the
interactive input, and the cdr destructures the cdr of INTERACTIVE argtype
forms.

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
  (assert (and (listp args) (< 0 (length args))) nil 'invalid-command-type :name name :args args)
  `(setf ,(if (atom name) 
              `(command-type ,(keywordicate name))
              `(command-type ,(keywordicate (second name)) (cdr (or (command-table ,(keywordicate (car name))) (make-commands ,(keywordicate (car name)))))))
         (lambda ,args
           ,@body)))

;; IO
(deffmt fmt-command "~(~A~)~@[ ~{~S~^ ~}~]" "Format a COMMAND string given a name and list of args.")

;; arg parsing
(defun read-arg (input) (read (if (stringp input) (make-string-input-stream input) input) input nil))
(defun read-args (input) (read-lisp-until-end (if (stringp input) (make-string-input-stream input) input)))

(defgeneric parse-args (self input)
  (:documentation "Parse INPUT as the arguments to a call to SELF."))

(defun read-command (&optional (stream *standard-input*))
  (with-input-from-string (s (read-line stream))
    (destructuring-bind (c &rest args) (read-lisp-until-end s)
      (let ((cmd (or (command c) (undefined-command c args))))
        (values cmd args (function-lambda-list cmd) (interactive cmd))))))

(defun write-command (cmd &optional args (stream *standard-output*))
  (fmt-command stream cmd args)
  (fresh-line stream))

(defun parse-command (str &rest args)
  "Parse a COMMAND from STR."
  (with-input-from-string (s (if args 
                                 (with-output-to-string (v) 
                                   (fmt-command v (string str) args))
                                 str))
    (read-command s)))

(defun call-interactively (command &optional input)
  "Parse COMMAND's arguments from input according to its lambda-list and itype,
then execute it."
  (declare ((or string symbol) command))
  (catch 'cmd
    (if-let ((cmd (command command)))
      (call cmd (parse-args cmd input))
      (multiple-value-bind (cmd args ll) (apply 'parse-command
                                                command
                                                (with-input-from-string (s input) 
                                                  (read-lisp-until-end s)))
        ;; TODO 2026-01-22:
        (call cmd (parse-args cmd args))))))

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

(defun load-commands (name)
  (with-commands name
    (setq *command-types* *command-types*
          *commands* *commands*)))

(defkernel command (kernel-object)
  ((interactive :initarg :interactive :initform nil :reader interactive))
  (:documentation "Commands are INTERACTIVE-FUNCTIONs or instances of this class.

The INTERACTIVE declaration corresponds to the slot of the same name in this
class - both may be used to specify the ITYPE (interaction typespec) of the
command."))

(defmethod initialize-instance :after ((self command) &key kernel)
  (when kernel
    (setf (kernel self) kernel)))

(defmethod make-load-form ((self command) &optional env)
  (make-load-form-saving-slots self :slot-names '(interactive) :environment env))

(definline commandp (cmd) 
  (declare (values boolean))
  (and (or (typep cmd 'command)
           (command cmd))
       t))

;; internal method
(defmethod sb-impl::object-type-string ((self command)) "command function")

;; TODO 2026-01-22: apply-itype
(defmethod parse-args ((self command) (input list)) input)
(defmethod parse-args ((self command) (input string))
  (with-input-from-string (s input)
    (read-lisp-until-end s)))
(defmethod parse-args ((self command) (input stream))
  (read-lisp-until-end input))

(defmethod call ((self command) (args list))
  (apply self args))
(defmethod call ((self command) (args string))
  (apply self (parse-args self args)))
(defmethod call ((self string) (args list))
  (multiple-value-bind (cmd args) (apply 'parse-command self args)
    (call cmd args)))

(defmethod exec ((self command)) (funcall (kernel self)))
(defmethod exec ((self string)) (multiple-value-bind (cmd args) (parse-command self) (call cmd args)))

#+nil
(defgeneric command-parser (self input)
  (:documentation "Return a function which supplies the arguments for command SELF given INPUT."))
#+nil
(defgeneric command-pipe (self output)
  (:documentation "Pipe the output of command SELF to OUTPUT."))

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
  (check-type name (or symbol list))
  (multiple-value-bind (forms decl doc) (parse-body body :documentation t)
    (let* ((%name (if (atom name) (keywordicate name) (keywordicate (second name))))
           (%cmd* `(command ',%name ,@(unless (atom name)
                                      `((car (or (command-table ,(keywordicate (car name)))
                                                 (make-commands ,(keywordicate (car name)))))))))
           (%int (when decl (cdr (assoc 'interactive (cdar decl)))))) ;; interactive typespec
      ;; TODO 2026-01-22: 
      ;; (check-itype %int args) ; validate
      (with-gensyms (%cmd %kernel)
        `(let ((,%cmd (make-instance *command-class* 
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
                                   ;; TODO 2026-01-20: input stream to command-type
                                   (collect `(funcall (command-type ,name) ,@args)))))
                             %int))))
               (,%kernel (,@(if *command-names-p* `(defun ,(intern (string %name))) '(lambda))
                          ,args
                          ,@decl
                          (let ((*interactive* ,*interactive*)
                                (*command* ,%name))
                            (funcall *command-hook* :pre *command*)
                            (multiple-value-prog1 (progn ,@forms)
                              (funcall *command-hook* :post *command*))))))
           (setf (kernel ,%cmd) ; set the kernel slot of this COMMAND instance
                 ,(if *command-names-p* `(symbol-function ,%kernel) %kernel)
                 ,@(when doc `((kernel-documentation ,%cmd) ,doc))
                 ,%cmd* ,%cmd)
           ;; make aliases
           ,@(when (and (consp name) (cddr name))
               (mapcar (lambda (x) `(command-alias ',x ',(second name))) (cddr name))))))))

#+nil
(progn
  (define-command-type :test (in))
  (defcommand art (a b &optional c) (declare (interactive :test :test &optional :test)) (values a b c)))

(defun run-commands (&rest commands)
  "Run each command in COMMANDS sequentially."
  (loop for c in commands do (exec c)))

(defun call-command (cmd &rest args)
  "Shorthand for (CALL (COMMAND CMD) ARGS). CMD is evaluated."
  (call (command cmd) args))

(defmacro cmd (cmd &rest args)
  "Shorthand for (CALL (COMMAND CMD) ARGS). CMD is unevaluated."
  `(call (command ',cmd) '(,@args)))

(defun eval-command (cmd &optional interactive)
  "Execute CMD and print the RESULT. When INTERACTIVE is non-nil then
CALL-INTERACTIVELY is used. The :eval hook of the *COMMAND-HOOK* is called
with each hook being passed the RESULT."
    (let ((result
            ;; this fancy footwork lets us grab the backtrace from where the
            ;; error actually happened.
            (restart-case
                (handler-bind
                    ((error (lambda (c)
                              (invoke-restart 'eval-command-error
                                              (format nil "Error In Command '~a': ~A"
                                                      cmd c)))))
                  (let ((*interactive* interactive))
                    (exec cmd)))
              (eval-command-error (err-text)
                :interactive (lambda ()
                               (list (format nil "^B^1*Error In Command '^b~a^B'"
                                             cmd)))
                :report (lambda (s)
                          (format s "Exit command ~S" cmd))
                err-text))))
      (funcall *command-hook* :eval result)))
  
;;; Help Protocol
(defgeneric print-help (self &optional stream)
  (:documentation "Format command SELF as a helpful string.")
  (:method ((self command) &optional stream)
    (print-usage self stream)
    (when-let ((doc (kernel-documentation self)))
      (print doc stream))))

(defgeneric print-usage (self &optional stream)
  (:documentation "Format command SELF as a useful string.")
  (:method ((self command) &optional stream)
    (format stream "~@[~<~A~>~%~]" (kernel-documentation self))))

;;; Init
(defmethod init ((self (eql :commands)) &key name class copy (load t) names)
  (when class (setq *command-class* class))
  (setq *command-names-p* names)
  (when name
    (when copy (copy-commands copy name))
    (let ((cons (command-table name)))
      (if cons
          (when load
            (setq *commands* (car cons)
                  *command-types* (cdr cons)))
          (make-commands name))))
  (values *commands* *command-types*))

(defmethod reset ((self (eql :commands)) &key name)
  (if name (remhash name *command-table*)
      (setq *commands* (make-hash-table)
            *command-types* (make-hash-table)
            *command-class* 'command)))
