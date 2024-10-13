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
         :accessor opts :type (vector cli-opt))
   (cmds :initarg :cmds :initform (make-array 0 :element-type 'cli-cmd :adjustable t)
         :accessor cmds :type (vector cli-cmd))
   (thunk :initform 'default-thunk :initarg :thunk :accessor cli-thunk :type symbol)
   (lock :initform nil :initarg :lock :accessor cli-lock-p :type boolean)
   (description :initarg :description :accessor cli-description :type string)
   (args :initform nil :initarg :args :accessor cli-args))
  (:documentation "CLI command class inherited by both the 'main' command which is executed when
a CLI is called without arguments, and all subcommands."))

(defmethod initialize-instance :after ((self cli-cmd) &key)
  (with-slots (name thunk opts cmds) self
    (unless (stringp name) (setf name (format nil "~(~A~)" name)))
    (unless (vectorp cmds) (setf cmds (make-cmds cmds)))
    (unless (vectorp opts) (setf opts (make-opts opts)))
    self))

(defmethod make-load-form ((obj cli-cmd) &optional env)
  (make-load-form-saving-slots 
   obj 
   :slot-names '(name opts cmds thunk lock description args)
   :environment env))

(defmethod print-object ((self cli-cmd) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :active ~a :opts ~A :cmds ~A :args ~A"
            (cli-name self)
            (cli-lock-p self)
            (length (opts self))
            (length (cmds self))
            (length (cli-args self)))))

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
  (vector-push self (cmds place)))

(defmethod push-opt ((self cli-opt) (place cli-cmd))
  (vector-push self (opts place)))

(defmethod pop-cmd ((self cli-cmd))
  (vector-pop (cmds self)))

(defmethod pop-opt ((self cli-opt))
  (vector-pop (opts self)))

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
  (when-let ((c (find name (cmds self) :key #'cli-name :test #'string=)))
    (if active 
        ;; maybe issue warning here? report to user
        (when (cli-lock-p c)
          c)
        c)))

(defmethod (setf find-cmd) ((new cli-cmd) (self cli-cmd) name &optional active)
  (let ((match (find-cmd self name active) ))
    (substitute new match (cmds self) :test 'cli-equal)))

(defmethod active-cmds ((self cli-cmd))
  (remove-if-not #'cli-lock-p (cmds self)))

(defmethod activate-cmd ((self cli-cmd))
  (setf (cli-lock-p self) t))

(defmethod find-opts ((self cli-cmd) name &key active recurse)
  (let ((ret))
    (flet ((%find (o obj)
             (when-let ((found (find o (opts obj) :key #'cli-opt-name :test 'equal)))
               (push found ret))))
      (when (and recurse (cmds self))
        (loop for c across (cmds self)
              do (%find name c)))
      (%find name self)
      (when active
        (setf ret (remove-if-not #'cli-lock-p ret)))
      ret)))

(defmethod find-opt ((self cli-cmd) name &optional active)
  (let ((ret (find name (opts self) :key #'cli-opt-name :test 'equal)))
    (if active
        (when (cli-opt-lock ret) ret)
        ret)))

(defmethod (setf find-opt) ((new cli-opt) (self cli-cmd) name &optional active)
  (let ((match (find-opt self name active)))
    (substitute new match (opts self) :test 'cli-equal)))

(defmethod active-opts ((self cli-cmd))
  (remove-if-not 'cli-opt-lock (opts self)))

(defun find-short-opts (flag cmd &key recurse)
  "Find and return all CLI-OPTs matching character or string FLAG in CMD.

- recurse :: optionally check nested commands as well."
  (let ((ret))
    (flet ((%find (ch obj)
             (when-let ((found (find (coerce ch 'character) obj 
                                     :key #'cli-opt-name 
                                     :test #'opt-string-prefix-eq)))
               (push found ret))))
      (flet ((%recurse-ch (ch vec)
               (loop for c across vec
                     do (%find ch (opts c))))
             (%recurse-str (str vec)
               (loop for c across vec
                     for ch across str
                     do (%find ch (opts c)))))
        (etypecase flag
          (character
           (when recurse (%recurse-ch flag (cmds cmd)))
           (%find flag (opts cmd)))
          (string
           (when recurse (%recurse-str flag (cmds cmd)))
           (%find flag (opts cmd))))
        ret))))

(declaim (inline solop))
(defun solop (self)
  (= 0 (length (active-cmds self)) (length (active-opts self))))

(defmacro with-opt-restart-case (arg condition)
  "Bind restarts 'use-as-arg' and 'discard-arg' for duration of BODY."
  `(restart-case ,condition
     (use-as-arg () () (make-cli-node 'arg ,arg))
     (discard-arg () () (setf ,arg nil))))

(defmethod proc-args ((self cli-cmd) args)
  "Process ARGS into an ast. Each element of the ast is a node with a
:kind slot, indicating the type of node and a :form slot which stores
an object."
  (make-cli-ast
   (flatten
    (loop
      with skip
      with exit
      for (a . args) on args
      if skip
      do (setq skip nil)
      else if exit
      do (loop-finish)
      else if (short-opt-p a) ;; SHORT OPT
      collect
         (let* ((has-eq (short-opt-has-eq-p a))
                (names (or (car has-eq) (string-left-trim "-" a)))
                (opts (find-short-opts names self :recurse nil)))
           (cond
             ((and (= (length opts) 1) (not has-eq))
              (let ((o (car opts)))
                (if (eql (cli-opt-kind o) 'boolean)
                    (%compose-flag-opt o)
                    (prog1
                        (%compose-value-opt o (pop args))
                      (setq skip t)))))
             ((and has-eq opts)
              (loop for o in opts
                    do (setf (cli-opt-val o) (cdr has-eq))
                    collect (make-cli-node 'opt o)))
             ((and (not has-eq) opts)
              (loop for o in opts
                    collect (%compose-flag-opt o)))
             (t
              (with-opt-restart-case a
                (clap-unknown-argument a 'cli-opt)))))
      else if (long-opt-p a) ;; LONG OPT
      collect           
         (let* ((has-eq (long-opt-has-eq-p a))
                (name (or (car has-eq) (string-left-trim "-" a)))
                (o (car (find-opts self name :recurse nil))))
           (cond
             ((and has-eq o)
              (setf (cli-opt-val o) (cdr has-eq))
              (make-cli-node 'opt o))
             ((and (not has-eq) o)
              (prog1
                  (%compose-value-opt o (pop args))
                (setq skip t)))
             (t ;; (not o) (not has-eq)
              (with-opt-restart-case a
                (clap-unknown-argument a 'cli-opt)))))
      ;; OPT GROUP
      else if (opt-group-p a)
      collect 
         (make-cli-node 'group nil)
      ;; OPT KEYWORD (experimental)
      else if (opt-keyword-p a)
      collect (if-let ((o (car (find-opts self (string-left-trim ":" a) :recurse nil))))
                (prog1 (%compose-keyword-opt o (pop args))
                  (setq exit t))
                (make-cli-node 'arg a))
      else ;; CMD or ARG
      collect
         (if-let ((cmd (find-cmd self a)))
           (prog1 (make-cli-node 'cmd (parse-args cmd args :install t))
             (setq exit t))
           ;; just a plain arg - move to next
           (make-cli-node 'arg a))))))

(defmethod install-ast ((self cli-cmd) (ast cli-ast))
  "Install the given AST, recursively filling in value slots."
  (with-slots (cmds opts) self
    ;; we assume all nodes in the ast have been validated and the ast
    ;; itself is consumed. validation is performed in proc-args.

    ;; before doing anything else we lock SELF, which should remain
    ;; locked until all subcommands have completed
    (activate-cmd self)
    (loop named install
          for (node . tail) on (ast ast)
          while node
          do 
             (let ((kind (cli-node-kind node)) 
                   (form (cli-node-form node)))
               (case kind
                 ;; opts
                 (opt
                  (setf (find-opt self (cli-name form)) form)
                  (activate-opt (find-opt self (cli-name form)))
                  (log:trace! (format nil "installing opt ~A" (cli-name form))))
                 (cmd
                  (setf (find-cmd self (cli-name form)) form)
                  (log:trace! (format nil "installing cmd ~A" (cli-name form))))
                 (arg (push-arg form self)))))
    (setf (cli-args self) (nreverse (cli-args self)))
    self))

(defmethod install-thunk ((self cli-cmd) (lambda function) &optional compile)
  "Install THUNK into the corresponding slot in cli-cmd SELF."
  (let ((%thunk (if compile (compile nil lambda) lambda)))
    (setf (cli-thunk self) %thunk)
    self))

(defmethod push-arg (arg (self cli-cmd))
  "Push an ARG onto the corresponding slot of a CLI-CMD."
  (push arg (cli-args self)))

(defmethod parse-args ((self cli-cmd) args &key (install t))
  "Parse ARGS and return the updated object SELF.
ARGS is assumed to be a valid cli-ast (list of cli-nodes), unless COMPILE is
t, in which case a list of strings is assumed. INSTALL always implies COMPILE
and calls INSTALL-AST on SELF with ARGS."
  (let ((args (proc-args self args)))
    (if install (install-ast self args)
        args)))

;; WARNING: make sure to fill in the opt and cmd slots with values
;; from the top-level args before calling a command.
(defmethod call-cmd ((self cli-cmd) args opts)
  (trace! "calling command:" args opts)
  (funcall (cli-thunk self) args opts))

(defmethod do-opts ((self cli-cmd))
  (do-opts (active-opts self)))

(defmethod do-cmd ((self cli-cmd))
  "Perform the active command or subcommand, recursively calling DO-CMD on
subcommands until a level is reached which satisfies SOLOP. active OPTS are
evaluated with DO-OPTS along the way."
  (do-opts self)
  (if (solop self)
      (prog1 (call-cmd self (cli-args self) (active-opts self))
        ;; release opts
        (loop for o across (active-opts self)
              do (setf (cli-opt-lock o) nil)))
      (loop for c across (active-cmds self)
            do (do-cmd c)))
  (setf (cli-lock-p self) nil))

