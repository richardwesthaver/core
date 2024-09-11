;;; cli/clap/cmd.lisp --- Clap Commands

;; Command Objects used to build CLI Applications.

;;; Commentary:

;; 

;;; Code:
(in-package :cli/clap/obj)

(defclass cli-cmd ()
  ;; name slot is required and must be a string
  ((name :initarg :name :initform (required-argument :name) :accessor cli-name :type string)
   (opts :initarg :opts :initform (make-array 0 :element-type 'cli-opt :adjustable t)
         :accessor cli-opts :type (vector cli-opt))
   (cmds :initarg :cmds :initform (make-array 0 :element-type 'cli-cmd :adjustable t)
         :accessor cli-cmds :type (vector cli-cmd))
   (thunk :initform #'default-thunk :initarg :thunk :accessor cli-thunk :type function-lambda-expression)
   (lock :initform nil :initarg :lock :accessor cli-lock-p :type boolean)
   (description :initarg :description :accessor cli-description :type string)
   (args :initform nil :initarg :args :accessor cli-cmd-args))
  (:documentation "CLI command class inherited by both the 'main' command which is executed when
a CLI is called without arguments, and all subcommands."))

(defmethod initialize-instance :after ((self cli-cmd) &key)
  (with-slots (name thunk opts cmds) self
    (unless (stringp name) (setf name (format nil "~(~A~)" name)))
    (unless (vectorp cmds) (setf cmds (make-cmds cmds)))
    (unless (vectorp opts) (setf opts (make-opts opts)))
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
    (format stream "~(~A~) ~A~A~A"
            (cli-name self)
            (if-let ((d (and (slot-boundp self 'description) (cli-description self))))
              (format nil ": ~A" d)
              "")
            (if (null opts)
                ""
                (format nil "~{~%    ~A~^~}" (loop for o across opts collect (print-usage o nil))))
            (if (null cmds)
                ""
                (format nil "~{!~A~}" (loop for c across cmds collect (print-usage c nil)))))))

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

(defmethod find-cmd ((self cli-cmd) name &optional active)
  (when-let ((c (find name (cli-cmds self) :key #'cli-name :test #'string=)))
    (if active 
        ;; maybe issue warning here? report to user
        (if (cli-lock-p c)
            c
            (clap-simple-error "inactive (unlocked) cmd: ~A" c))
        c)))

(defmethod active-cmds ((self cli-cmd))
  (remove-if-not #'cli-lock-p (cli-cmds self)))

(defmethod find-opts ((self cli-cmd) name &key active recurse)
  (let ((ret))
    (flet ((%find (o obj)
             (when-let ((found (find o (cli-opts obj) :key #'cli-opt-name :test 'equal)))
               (push found ret))))
      (when (and recurse (cli-cmds self))
        (loop for c across (cli-cmds self)
              do (%find name c)))
      (%find name self)
      (when active
        (setf ret (remove-if-not #'cli-lock-p ret)))
      ret)))

(defmethod active-opts ((self cli-cmd) &optional global)
  (remove-if-not 
   (if global 
       #'active-global-opt-p
       #'cli-opt-lock)
   (cli-opts self)))

(defmethod find-short-opts ((self cli-cmd) ch &key recurse)
  (let ((ret))
    (flet ((%find (ch obj)
             (when-let ((found (find ch (cli-opts obj) :key #'cli-opt-name :test #'opt-string-prefix-eq)))
               (push found ret))))
      (when (and recurse (cli-cmds self))
        (loop for c across (cli-cmds self)
              do (%find ch c)))
      (%find ch self)
      ret)))

(declaim (inline solop))
(defun solop (self)
  (and (= 0 (length (active-cmds self)) (length (active-opts self)))))

(defmacro with-opt-restart-case (arg condition)
  "Bind restarts 'use-as-arg' and 'discard-arg' for duration of BODY."
  `(restart-case ,condition
     (use-as-arg () () (make-cli-node 'arg ,arg))
     (discard-arg () () nil)))

(defmethod proc-args ((self cli-cmd) args)
  "Process ARGS into an ast. Each element of the ast is a node with a
:kind slot, indicating the type of node and a :form slot which stores
a value.

For now we parse group separators '--' and insert a nil into the tree,
this will likely change to generating a new branch in the ast as it
should be."
  (make-cli-ast
   (let ((holes)) ;; list of arg indexes which can be skipped since they're
                  ;; consumed by an opt
     (loop 
       for i below (length args)
       for (a . args) on args
       if (member i holes)
         do (continue) ;; skip args which have been consumed already
       ;; else
       ;;   if (= (length a) 1)
       ;;     collect (make-cli-node 'arg a) ; always treat single-char as arg
       else
         if (short-opt-p a) ;; SHORT OPT
           collect
           (if-let ((o (find-short-opts self (aref a 1) :recurse t)))
             (%compose-short-opt (car o) a)
             ;;  TODO 2024-09-11: signal error?
             (with-opt-restart-case a
               (clap-unknown-argument a)))
       else
         if (long-opt-p a) ;; LONG OPT
           collect           
             (let ((o (find-opts self (string-left-trim "-" a) :recurse t))
                   (has-eq (long-opt-has-eq-p a)))
               (cond
                 ((and has-eq o)
                  (setf (cli-opt-val o) (cdr has-eq))
                  (make-cli-node 'opt o))
                 ((and (not has-eq) o)
                  (prog1 (%compose-long-opt (car o) args)
                    (push (1+ i) holes)))
                 (t ;; (not o) (not has-eq)
                  (with-opt-restart-case a
                    (clap-unknown-argument a)))))
           ;; OPT GROUP
       else 
         if (opt-group-p a)
           collect nil
       ;; CMD
       else 
         collect
         (let ((cmd (find-cmd self a)))
           (if cmd
               ;; TBD
               (make-cli-node 'cmd (find-cmd self a))
               ;; ARG
               (make-cli-node 'arg a)))))))

(defmethod install-ast ((self cli-cmd) (ast cli-ast))
  "Install the given AST, recursively filling in value slots."
  (with-slots (cmds opts) self
    ;; we assume all nodes in the ast have been validated and the ast
    ;; itself is consumed. validation is performed in proc-args.

    ;; before doing anything else we lock SELF, which should remain
    ;; locked for the full runtime duration.
    (setf (cli-lock-p self) t)
    (loop named install
          for (node . tail) on (debug! (ast ast))
          until (null node)
          do 
             (let ((kind (cli-node-kind node)) (form (cli-node-form node)))
               (case kind
                 ;; opts 
                 (opt
                  (let ((name (cli-opt-name form)))
                    (when-let ((o (car (find-opts self name))))
                      (setf o form)
                      (setf (cli-opt-lock o) t))))
                 ;; when we encounter a command we recurse over the tail
                 (cmd 
                  (when-let ((c (find-cmd self (cli-name form))))
                    ;; handle the rest of the AST
                    (setf c (install-ast c (make-cli-ast tail)))
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
  "Push an ARG onto the corresponding slot of a CLI-CMD."
  (push arg (cli-cmd-args self)))

(defmethod parse-args ((self cli-cmd) args &key (compile t))
  "Parse ARGS and return the updated object SELF.
ARGS is assumed to be a valid cli-ast (list of cli-nodes), unless COMPILE is
t, in which case a list of strings is assumed."
  (with-slots (opts cmds) self
    (let ((args (if compile (proc-args self args) args)))
      (install-ast self args))))

;; WARNING: make sure to fill in the opt and cmd slots with values
;; from the top-level args before calling a command.
(defmethod call-cmd ((self cli-cmd) args opts)
  (trace! "calling command:" args opts)
  (funcall (cli-thunk self) args opts))

(defmethod do-cmd ((self cli-cmd))
  "Perform the command, recursively calling child commands and opts if necessary."
  (loop for o across (active-opts self)
        do (do-opt o))
  (if (solop self)
      (call-cmd self (cli-cmd-args self) (active-opts self))
      (loop for c across (active-cmds self)
            do (do-cmd c))))
  
