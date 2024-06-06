;;; ast.lisp --- Clap AST

;; 

;;; Code:

;; typically when starting from a top-level CLI, the global
;; CLI-OPTS will be parsed first, followed by the first command
;; found. If a command is found, the tail of the list is passed as
;; arguments to this function, which can pass additonal arguments to
;; nested commands.

;;  TODO 2023-09-12: Parsing restarts at the `*cli-group-separator*'
;; if present, or stops at EOI.

(defstruct (cli-node (:constructor make-cli-node (kind form))) kind form)

(defstruct (cli-ast (:constructor make-cli-ast (ast))) ast)

(defun %compose-short-opt (o arg)
  (declare (ignorable arg))
  (setf (cli-opt-val o) t)
  (make-cli-node 'opt o))

(defun %compose-long-opt (o args)
  (declare (ignorable args))
  (setf (cli-opt-val o) (or (pop args) t))
  (make-cli-node 'opt o))

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
       else if (= (length a) 1)
       collect (make-cli-node 'arg a) ; always treat single-char as arg
       else if (short-opt-p a) ;; SHORT OPT
       collect (if-let ((o (find-short-opts self (aref a 1) :recurse t)))
                 (%compose-short-opt (car o) a)
                 (make-cli-node 'arg a))
       else if (long-opt-p a) ;; LONG OPT
       collect (if-let ((o (find-opts self (string-left-trim "-" a) :recurse t)))
                 (prog1 (%compose-long-opt (car o) args)
                   (push (1+ i) holes))
                 (make-cli-node 'arg a))
       ;; OPT GROUP
       else if (opt-group-p a)
       collect nil
       ;; CMD
       else
       collect (let ((cmd (find-cmd self a)))
                 (if cmd
                     ;; TBD
                     (make-cli-node 'cmd (find-cmd self a))
                     ;; ARG
                     (make-cli-node 'arg a)))))))
