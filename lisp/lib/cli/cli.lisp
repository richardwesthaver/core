;;; cli.lisp --- cli programming api and utils

;; This package contains a simple api and macros for building lisp CLI
;; programs.

;;; Commentary:

;; - inspired by: clingon, uiop

;; Basic assumptions at runtime:
;;   - running in a POSIX-compliant shell
;;   - output stream supports UTF-8

;; TODO 2023-10-14: install-ast, install-thunk, proc-args, etc should
;; return IR types - CLI-IR THUNK and CLI-IR respectively.

;; TODO 2023-10-14: rename cli-ast to cli-ir, install-ast to
;; install-ir, etc.

;;; Code:
(pkg:defpkg :cli
  (:use :cl :sym :cond :fu :str :ana :fmt :log)
  (:import-from :ana :alet)
  (:import-from :uiop :println)
  (:import-from :sb-posix :filename-designator)
  (:import-from :sb-ext :parse-native-namestring)
  (:shadowing-import-from :sb-ext :exit)
  (:export
   :*argv*
   :init-args
   :cli-arg0
   :cli-args
   :command-line-args
   :*cli-group-separator*
   :*cli-opt-kinds*
   :global-opt-p
   :exec-path-list
   :argp
   :$val
   :$args
   :$argc
   :$opts
   :$optc
   :make-shorty
   :with-cli-handlers
   :completing-read
   :make-prompt!
   :defmain
   :main
   :with-cli
   :make-cli
   ;; opt-parsers
   :make-opt-parser
   :parse-bool-opt
   :parse-str-opt
   :parse-form-opt
   :parse-list-opt
   :parse-sym-opt
   :parse-key-opt
   :parse-num-opt
   :parse-str-opt
   :parse-file-opt
   :parse-dir-opt
   :make-opts
   :make-cmds
   :active-opts
   :active-cmds
   :proc-args
   :make-cli-node
   :make-cli-ast
   :proc-args
   :parse-args
   :debug-opts
   :do-cmd
   :do-opt
   :call-opt
   :call-cmd
   :apply-cmd
   :print-help
   :print-version
   :print-usage
   :handle-unknown-argument
   :handle-missing-argument
   :handle-invalid-argument
   :cli-opt
   :cli-val
   :cli-cmd-args
   :cli-cmd
   :cli-cwd
   :find-cmd
   :find-opt
   :find-short-opt
   :install-ast
   ;; :gen-cli-thunk
   :install-thunk
   :cli
   :cli-equal
   :defopt
   :defcmd
   :define-cli
   ;; ast types
   :opt
   :cmd
   :arg
   :cli-name
   :cli-opts
   :cli-cmds
   :cli-thunk
   :cli-description
   :cli-version
   :cli-usage))

(in-package :cli)

(defun cli-arg0 () (car sb-ext:*posix-argv*))
(defun cli-args () (cdr sb-ext:*posix-argv*))

(declaim (inline exec-path-list))
(defun exec-path-list ()
  (let ((var (sb-posix:getenv "PATH")))
    (mapcar #'directory
	    (loop for i = 0 then (1+ j)
		  as j = (position #\: var :start i)
		  collect (subseq var i j)
		while j))))

(defparameter *cli-group-separator*
  "--"
  "A marker specifying the end of a unique group of CLI args.")

;; uiop:command-line-arguments

;;; Macros
(defmacro argp (arg &optional (args (cli-args)))
  "Test for presence of ARG in ARGS. Return the tail of
ARGS starting from the position of ARG."
  `(member ,arg ,args :test #'string=))

(defmacro make-shorty (name)
  "Return the first char of symbol or string NAME."
  `(character (aref (if (stringp ,name) ,name (symbol-name ,name)) 0)))

;; (defun treat-as-argument (condition)
;;   "A handler which can be used to invoke the `treat-as-argument' restart"
;;   (invoke-restart (find-restart 'treat-as-argument condition)))

;; (defun discard-argument (condition)
;;   "A handler which can be used to invoke the `discard-argument' restart"
;;   (invoke-restart (find-restart 'discard-argument condition)))

(defmacro with-cli-handlers (form)
  "A wrapper which handles common cli errors that may occur during
evaluation of FORM."
  `(handler-case ,form
     (sb-sys:interactive-interrupt ()
       (format *error-output* "~&(:SIGINT)~&")
       (exit :code 130))
     (error (c)
       (format *error-output* "~&~A~&" c)
       (exit :code 1))))

(defun init-args () (setq *argv* (cons (cli-arg0) (cli-args))))

(defmacro with-cli (slots cli &body body)
  "Like with-slots with some extra bindings."
  ;; (with-gensyms (cli-body)
  ;;  (let ((cli-body (mapcar (lambda (x) ()) cli-body)
  `(progn
     (init-args)
     (setf (cli-cwd ,cli) (sb-posix:getcwd))
     (with-slots ,slots (parse-args ,cli *argv* :compile t)
       ,@body)))

;;; Prompts
(declaim (inline completing-read))
(defun completing-read (prompt collection
			&key (history nil) (default nil)
			(key nil) (test nil))

  "A simplified COMPLETING-READ for common-lisp.

The Emacs completion framework includes a function called
`completing-read' which prompts the user for input from the
mini-buffer. It is a very flexible interface which can be used to read
user input programatically. This is incredibly useful for building
data entry interfaces -- for example see the `make-prompt!' macro.

Obviously writing a completion framework is out-of-scope, but we can
simulate one by embedding a DSL in our prompters if we choose. For
example, perhaps we treat a single '?' character as a request from the
user to list valid options while continue waiting for input."
  (princ prompt)
  ;; ensure we empty internal buffer
  (finish-output)
  (let* ((coll (symbol-value collection))
	 (r (if coll
		(find (read-line) coll :key key :test test)
		(or (read-line) default))))
    (prog1
	r
    (setf (symbol-value history) (push r history)))))

(defmacro make-prompt! (var &optional prompt)
  "Generate a 'prompter' from list or variable VAR and optional
PROMPT string.

This isn't an ideal solution as it does in fact expose a dynamic
variable (VAR-prompt-history). We should generate accessors and
keep the variables within lexical scope of the generated
closure."
  (with-gensyms (s p h)
    `(let ((,s (if (boundp ',var) (symbol-value ',var) 
		   (progn 
		     (defvar ,(symb var) nil)
		     ',(symb var))))
           (,p (when (stringp ,prompt) ,prompt)) ;; prompt string
           (,h ',(symb var '-prompt-history))) ;; history symbol
       (defvar ,(symb var '-prompt-history) nil)
       (defun ,(symb var '-prompt) ()
	 ,(format nil "Prompt for a value from `~A', use DEFAULT if non-nil
and no value is provided by user, otherwise fallback to the `car'
of `~A-PROMPT-HISTORY'." var var)
	 (completing-read
          (format nil "~A [~A]: "
		  (or ,p ">")
		  (car (symbol-value ,h)))
	  ,s :history ,h :default nil)))))

(defmacro define-cli-constant (name cli &optional doc)
  `(define-constant ,name ,cli ,@doc :test #'cli-equal))

(defvar *default-cli-def* 'defparameter)

(defmacro defcmd (name &body body)
  `(defun ,name ($args $opts) 
     (declare (ignorable $args $opts))
     (let (($argc (length $args))
	   ($optc (length $opts)))
       (declare (ignorable $argc $optc))
       ,@body)))

(defmacro defopt (name &body body)
  `(defun ,name ($val)
     (declare (ignorable $val))
     ,@body))

(declaim (inline walk-cli-slots))
(defun walk-cli-slots (cli)
  "Walk the plist CLI, performing actions as necessary based on the slot
keys."
  (loop for kv in (group cli 2)
	when (eql :thunk (car kv))
	  return (let ((th (cdr kv)))
		   (if (or (functionp th) (symbolp th)) (funcall th) (compile nil (lambda () th)))))
	cli)

(defmacro define-cli (name &body body)
  "Define a symbol NAME bound to a top-level CLI object."
  (declare (type symbol name))
      `(,*default-cli-def* ,name (apply #'make-cli t (walk-cli-slots ',body))))

(defmacro defmain (ret &body body)
  "Define a main function in the current package which returns RET.

Note that this macro does not export the defined function and requires
`cli:main' to be an external symbol."
  `(progn
     (declaim (type stream output))
     (defun main (&key (output *standard-output*))
       "Run the top-level function and print to OUTPUT."
       (let ((*standard-output* output))
	 (with-cli-handlers
	     (progn ,@body ,ret))))))

;;; Utils
(defvar *argv*)

(defun make-cli (kind &rest slots)
  "Creates a new CLI object of the given kind."
  (declare (type (member :opt :cmd :cli t) kind))
  (apply #'make-instance
   (cond
     ((eql kind :cli) 'cli)
     ((eql kind :opt) 'cli-opt)
     ((eql kind :cmd) 'cli-cmd)
     (t 'cli))
    slots))

;; RESEARCH 2023-09-12: closed over hash-table with short/long flags
;; to avoid conflicts. if not, need something like a flag-function
;; slot at class allocation.
(defmacro make-opts (&body opts)
  `(map 'vector
	(lambda (x)
	  (etypecase x
	    (string (make-cli :opt :name x))
	    (list (apply #'make-cli :opt x))
	    (t (make-cli :opt :name (format nil "~(~A~)" x) :global t))))
	(walk-cli-slots ',opts)))

(defmacro make-cmds (&body opts)
  `(map 'vector
	(lambda (x)
	  (etypecase x
	    (string (make-cli :cmd :name x))
	    (list (apply #'make-cli :cmd x))
	    (t (make-cli :cmd :name (format nil "~(~A~)" x)))))
	(walk-cli-slots ',opts)))

(defun long-opt-p (str)
  (and (char= (aref str 0) (aref str 1) #\-)
       (> (length str) 2)))

(defun short-opt-p (str)
  (and (char= (aref str 0) #\-)
       (not (char= (aref str 1) #\-))
       (> (length str) 1)))

(defun opt-group-p (str)
  (string= str *cli-group-separator*))

(defun opt-prefix-eq (ch str)
  (char= (aref str 0) ch))

(defun gen-thunk-ll (origin args)
  (let ((a0 (list (symb '$a 0) origin)))
    (group 
     (nconc (loop for i from 1 for a in args nconc (list (symb '$a i) a)) a0 )
     2)))

;; TODO 2023-10-06: 
;; (defmacro gen-cli-thunk (pvars &rest thunk)
;;   "Generate and return a function based on THUNK suitable for the :thunk
;; slot of cli objects with pandoric bindings PVARS.")

;;; Protocol
(defgeneric push-cmd (cmd place))

(defgeneric push-opt (opt place))

(defgeneric pop-cmd (place))

(defgeneric pop-opt (place))

(defgeneric find-cmd (self name &optional active))

(defgeneric find-opt (self name &optional active))

(defgeneric active-cmds (self))

(defgeneric active-opts (self &optional global))

(defgeneric find-short-opt (self ch))

(defgeneric call-opt (self arg))

(defgeneric do-opt (self))

(defgeneric call-cmd (self args opts))

(defgeneric parse-args (self args &key &allow-other-keys)
  (:documentation "Parse list of strings ARGS using SELF.

A list of the same length as ARGS is returned containing 'cli-ast'
objects: (OPT . (or char string)) (CMD . string) NIL"))

(defgeneric do-cmd (self)
  (:documentation "Run the command SELF with args parsed at runtime."))

(defgeneric print-help (self &optional stream)
  (:documentation "Format cli SELF as a helpful string."))

(defgeneric print-version (self &optional stream)
  (:documentation "Print the version of SELF."))

(defgeneric print-usage (self &optional stream)
  (:documentation "Format cli SELF as a useful string."))

(defgeneric handle-unknown-argument (self arg)
  (:documentation "Handle an unknown argument."))

(defgeneric handle-missing-argument (self arg)
  (:documentation "Handle a missing argument."))

(defgeneric handle-invalid-argument (self arg)
  (:documentation "Handle an invalid argument."))

(defgeneric cli-equal (a b))

(defun default-thunk (cli) (lambda (x) (declare (ignore x)) (print-help cli)))

(defvar *cli-opt-kinds* '(bool str form list sym key num file dir))

(defun cli-opt-kind-p (s)
  (declare (type symbol s))
  (find s *cli-opt-kinds*))

(defmacro make-opt-parser (kind-spec &body body)
  "Return a KIND-opt-parser function based on KIND-SPEC which is either a
symbol from *cli-opt-kinds* or a list, and optional BODY which
is a list of handlers for the opt-val."
  (let* ((kind (if (consp kind-spec) (car kind-spec) kind-spec))
	 (super (when (consp kind-spec) (cadr kind-spec)))
	 (fn-name (symb 'parse- kind '-opt)))
    ;; thread em
    (let ((fn1 (when (not (eql 'nil super)) (symb 'parse- super '-opt))))
      `(progn
	 (defun ,fn-name ($val)
	   "Parse the cli-opt-val $VAL."
	   ;; do stuff
	   (when (not (eql ',fn1 'nil)) (setq $val (funcall ',fn1 $val)))
	   ,@body)))))

(make-opt-parser bool $val)

(make-opt-parser (str bool) (when (stringp $val) $val))

(make-opt-parser (form str) (read-from-string $val))

(make-opt-parser (list form) (when (listp $val) $val))

(make-opt-parser (sym form) (when (symbolp $val) $val))

(make-opt-parser (key form) (when (keywordp $val) $val))

(make-opt-parser (num form) (when (numberp $val) $val))

(make-opt-parser (file str) 
  (when $val (parse-native-namestring $val nil *default-pathname-defaults* :as-directory nil)))

(make-opt-parser (dir str) 
  (when $val (sb-ext:parse-native-namestring $val nil *default-pathname-defaults* :as-directory t)))

;;; Objects
(defclass cli-opt ()
  ;; note that cli-opts can have a nil or unbound name slot
  ((name :initarg :name :initform (required-argument :name) :accessor cli-name :type string)
   (kind :initarg :kind :initform 'boolean :accessor cli-opt-kind :type cli-opt-kind-p)
   (thunk :initform #'default-thunk :initarg :thunk :type function-lambda-expression :accessor cli-thunk)
   (val :initarg :val :initform nil :accessor cli-val :type form)
   (global :initarg :global :initform nil :accessor global-opt-p :type boolean)
   (description :initarg :description :accessor cli-description :type string)
   (lock :initform nil :initarg :lock :accessor cli-lock-p :type boolean))
  (:documentation "CLI option"))

(defmethod handle-unknown-argument ((self cli-opt) arg))
(defmethod handle-missing-argument ((self cli-opt) arg))
(defmethod handle-invalid-argument ((self cli-opt) arg))

(defmethod initialize-instance :after ((self cli-opt) &key)
  (with-slots (name thunk) self
    (unless (stringp name) (setf name (format nil "~(~A~)" name)))
    (when (symbolp thunk) (setf thunk (funcall (compile nil `(lambda () ,(symbol-function thunk))))))
    self))

(defmethod install-thunk ((self cli-opt) (lambda function) &optional compile)
  "Install THUNK into the corresponding slot in cli-cmd SELF."
  (let ((%thunk (if compile (compile nil lambda) lambda)))
    (setf (cli-thunk self) %thunk)
    self))

(defmethod print-object ((self cli-opt) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :global ~A :val ~A"
            (cli-name self)
	    (global-opt-p self)
	    (cli-val self))))

(defmethod print-usage ((self cli-opt) &optional stream)
  (format stream " -~(~{~A~^/--~}~)~A~A"
	  (if-let ((n (cli-name self)))
	    (list (make-shorty n) n)
	    'dyn)
	  (if (global-opt-p self) "* " "  ")
	  (if-let ((d (and (slot-boundp self 'description) (cli-description self))))
	    (format stream ":  ~A" d)
	    "")))

(defmethod cli-equal ((a cli-opt) (b cli-opt))
  (with-slots (name global kind) a
    (with-slots ((bn name) (bg global) (bk kind)) b
      (and (string= name bn)
	   (eql global bg)
	   (eql kind bk)))))

(defmethod call-opt ((self cli-opt) arg)
  (funcall (compile nil (cli-thunk self)) arg))

(defmethod do-opt ((self cli-opt))
  (call-opt self (cli-val self)))

(defclass cli-cmd ()
  ;; name slot is required and must be a string
  ((name :initarg :name :initform (required-argument :name) :accessor cli-name :type string)
   (opts :initarg :opts :initform (make-array 0 :element-type 'cli-opt)
	 :accessor cli-opts :type (vector cli-opt))
   (cmds :initarg :cmds :initform (make-array 0 :element-type 'cli-cmd)
	 :accessor cli-cmds :type (vector cli-cmd))
   (thunk :initform #'default-thunk :initarg :thunk :accessor cli-thunk :type function-lambda-expression)
   (lock :initform nil :initarg :lock :accessor cli-lock-p :type boolean)
   (description :initarg :description :accessor cli-description :type string)
   (args :initform nil :initarg :args :accessor cli-cmd-args))
  (:documentation "CLI command"))

(defmethod initialize-instance :after ((self cli-cmd) &key)
  (with-slots (name cmds opts thunk) self
    (unless (stringp name) (setf name (format nil "~(~A~)" name)))
    (unless (vectorp cmds) (setf cmds (funcall (compile nil `(lambda () ,cmds)))))
    (unless (vectorp opts) (setf opts (funcall (compile nil `(lambda () ,opts)))))
    (when (symbolp thunk) (setf thunk (symbol-function thunk)))
    self))

(defmethod print-object ((self cli-cmd) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :opts ~A :cmds ~A :args ~A"
	  (cli-name self)
          (length (cli-opts self))
	  (length (cli-cmds self))
	  (length (cli-cmd-args self)))))

(defmethod print-usage ((self cli-cmd) &optional stream)
  (with-slots (opts cmds) self
    (format stream "~(~A~)  ~A~A~A"
	    (cli-name self)
	    (if-let ((d (and (slot-boundp self 'description) (cli-description self))))
	      (format nil ":  ~A" d)
	      "")
	    (if (null opts)
		""
		(format nil "~{~%    ~A~^~}" (loop for o across opts collect (print-usage o nil))))
	    (if (null cmds)
		""
		(format nil "~%    ~{!  ~A~}" (loop for c across cmds collect (print-usage c nil)))))))

(defmethod push-cmd ((self cli-cmd) (place cli-cmd))
  (vector-push self (cli-cmds place)))

(defmethod push-opt ((self cli-opt) (place cli-cmd))
  (vector-push self (cli-opts place)))

(defmethod pop-cmd ((self cli-cmd))
  (vector-pop (cli-cmds self)))

(defmethod pop-opt ((self cli-opt))
  (vector-pop (cli-opts self)))

(defmethod cli-equal ((a cli-cmd) (b cli-cmd))
  (with-slots (name opts cmds) a
    (with-slots ((bn name) (bo opts) (bc cmds)) b
      (and (string= name bn)
	   (if (and (null opts) (null bo))
	       t
	       (unless (member nil (loop for oa across opts
					 for ob across bo
					 collect (cli-equal oa ob)))
		 t))
	   (if (and (null cmds) (null bc))
	       t
	       (unless (member nil (loop for ca across cmds
					 for cb across bc
					 collect (cli-equal ca cb)))
		 t))))))

;; typically when starting from a top-level `cli', the global
;; `cli-opts' will be parsed first, followed by the first command
;; found. If a command is found, the tail of the list is passed as
;; arguments to this function, which can pass additonal arguments to
;; nested commands.

;;  TODO 2023-09-12: Parsing restarts at the `*cli-group-separator*'
;; if present, or stops at EOI.

(declaim (inline %make-cli-node))
(defstruct (cli-node (:constructor %make-cli-node)) kind form)

(defun make-cli-node (kind form)
  (%make-cli-node :kind kind :form form))

(declaim (inline %make-cli-ast))
(defstruct (cli-ast (:constructor %make-cli-ast)) ast)

(defun make-cli-ast (nodes)  
  (%make-cli-ast :ast nodes))

(defmethod find-cmd ((self cli-cmd) name &optional active)
  (when-let ((c (find name (cli-cmds self) :key #'cli-name :test #'string=)))
    (if active 
	;; maybe issue warning here? report to user
	(when (cli-lock-p c) c)
	c)))

(defmethod active-cmds ((self cli-cmd))
  (remove-if-not #'cli-lock-p (cli-cmds self)))


(defmethod find-opt ((self cli-cmd) name &optional active)
  (when-let ((o (find name (cli-opts self) :key #'cli-name :test #'string=)))
    (if active 
	(when (cli-lock-p o) o)
	o)))

(defun active-global-opt-p (opt)
  "Return non-nil if OPT is active at runtime and global."
  (when (and (cli-lock-p opt) (global-opt-p opt)) t))

(defmethod active-opts ((self cli-cmd) &optional global)
  (remove-if-not 
   (if global 
       #'active-global-opt-p 
       #'cli-lock-p)
   (cli-opts self)))

(defmethod find-short-opt ((self cli-cmd) ch)
  (find ch (cli-opts self) :key #'cli-name :test #'opt-prefix-eq))

(defmethod proc-args ((self cli-cmd) args)
  "process ARGS into an ast. Each element of the ast is a node with a
:kind slot, indicating the type of node and a :form slot which stores
a value.

For now we parse group separators '--' and insert a nil into the tree,
this will likely change to generating a new branch in the ast as it
should be."
  (make-cli-ast
   (loop 
     for a in args
     if (= (length a) 1) collect (make-cli-node 'arg a)
     ;; SHORT OPT
     else if (short-opt-p a)
	    collect (if-let ((o (find-short-opt self (aref a 1))))
		      (progn
			(setf (cli-val o) t)
			(make-cli-node 'opt o))
		      (make-cli-node 'arg a))

     ;; LONG OPT
     else if (long-opt-p a)
	    ;; what we actually want to do is consume the next sequence of args - TBD
	    collect (if-let ((o (find-opt self (string-trim "-" a))))
		      (progn
			(setf (cli-val o) (string-trim "-" a))
			(make-cli-node 'opt o))
		      (make-cli-node 'arg a))
     ;; OPT GROUP
     else if (opt-group-p a)
	    collect nil
     ;; CMD
     else if (find-cmd self a)
	    ;; TBD
	    collect (make-cli-node 'cmd (find-cmd self a))
     ;; ARG
     else collect (make-cli-node 'arg a))))

(defmethod install-ast ((self cli-cmd) (ast cli-ast))
  "Install the given AST, recursively filling in value slots."
  (with-slots (cmds opts) self
    ;; we assume all nodes in the ast have been validated and the ast
    ;; itself is consumed. validation is performed in proc-args.

    ;; before doing anything else we lock SELF, which should remain
    ;; locked for the full runtime duration.
    (setf (cli-lock-p self) t)
    (loop named install
	  for (node . tail) on (cli-ast-ast ast)
	  unless (null node) 
	    do 
	       (with-slots (kind form) node
		 (case kind
		   ;; opts 
		   (opt 
		    (let ((name (cli-name form))
			  (val (cli-val form)))
		      (when-let ((o (find-opt self name)))
			(setf (cli-val o) val
			      (cli-lock-p o) t))))
		   ;; when we encounter a command we recurse over the tail
		   (cmd 
		    (when-let ((c (find-cmd self (cli-name form))))
		      (setf (cli-lock-p c) t)
		      ;; handle the rest of the AST
		      (install-ast c (make-cli-ast tail))
		      (return-from install)))
		   (arg (push-arg form self)))))
    (setf (cli-cmd-args self) (nreverse (cli-cmd-args self)))
    self))

(defmethod install-thunk ((self cli-cmd) (lambda function) &optional compile)
  "Install THUNK into the corresponding slot in cli-cmd SELF."
  (let ((%thunk (if compile (compile nil lambda) lambda)))
    (setf (cli-thunk self) %thunk)
    self))

(defmethod push-arg (arg (self cli-cmd))
  (push arg (cli-cmd-args self)))

(defmethod parse-args ((self cli-cmd) args &key (compile nil))
  "Parse ARGS and return the updated object SELF.

ARGS is assumed to be a valid cli-ast (list of cli-nodes), unless
COMPILE is t, in which case a list of strings is assumed."
  (with-slots (opts cmds) self
    (let ((args (if compile (proc-args self args) args)))
      (print (install-ast self args)))))

;; warning: make sure to fill in the opt and cmd slots with values
;; from the top-level args before doing a command.
(defmethod call-cmd ((self cli-cmd) args opts)
  ;; TODO 2023-09-12: handle args/env
  (funcall (cli-thunk self) args opts))

(defmethod do-cmd ((self cli-cmd))
  (call-cmd self (cli-cmd-args self) (cli-opts self)))

(defclass cli (cli-cmd)
  ;; name slot defaults to *package*, must be string
  ((name :initarg :name :initform (string-downcase (package-name *package*)) :accessor cli-name :type string)
   (version :initarg :version :initform "0.1.0" :accessor cli-version :type string)
   ;; TODO 2023-10-11: look into pushd popd - wd-stack?
   (cwd :initarg :cwd :initform (sb-posix:getcwd) :type string :accessor cli-cwd
	:documentation "working directory of the top-level CLI."))
  (:documentation "CLI"))

(defmethod print-usage ((self cli) &optional stream)
  (iprintln (format nil "usage: ~A [global] <command> [<arg>]~%" (cli-name self)) 2 stream))

(defmethod print-version ((self cli) &optional stream)
  (println (cli-version self) stream))

(defmethod print-help ((self cli) &optional stream) 
  (println (format nil "~A v~A" (cli-name self) (cli-version self)) stream)
  (print-usage self stream)
  (iprintln (cli-description self) 2 stream)
  ;; (terpri stream)
  (iprintln "options:" 2 stream)
  (with-slots (opts cmds) self
    (unless (null opts)
      (loop for o across opts
	    do (iprintln (print-usage o) 4 stream)))
    ;; (terpri stream)
    (iprintln "commands:" 2 stream)
    (unless (null cmds)
      (loop for c across cmds
	    do (iprintln (print-usage c) 4 stream)))))

(defmethod cli-equal :before ((a cli) (b cli))
  "Return T if A is the same cli object as B.

Currently this function is intended only for instances of the CLI
class and is used as a specialized EQL for DEFINE-CONSTANT."
  (with-slots (version) a
    (with-slots ((bv version)) b
      (string= version bv))))

;; same as cli-cmd method, default is to compile though
(defmethod parse-args ((self cli) (args list) &key (compile t))
  "Parse list of string arguments ARGS and return the updated object SELF."
  (with-slots (opts cmds) self
    (let ((args (if compile (proc-args self args) args)))
      (install-ast self args))))

(declaim (inline debug-opts))
(defun debug-opts (cli)
  (let ((o (active-opts cli))
	(a (cli-cmd-args cli))
	(c (active-cmds cli)))
    (debug! (cli-cwd cli) o a c)))

(declaim (inline solop))
(defun solop (self)
  (and (= 0 (length (active-opts self t)) (length (active-cmds self)))))

(defmethod do-cmd ((self cli))
  (if (solop self)
      (call-cmd self (cli-cmd-args self) (cli-opts self))
      (progn
	(loop for o across (active-opts self t)
	      do (do-opt o))
	(loop for c across (active-cmds self)
	      do (do-cmd c)))))

(provide :cli)
