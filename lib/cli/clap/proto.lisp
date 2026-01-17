;;; cli/clap/proto.lisp --- Clap Protocol

;; 

;;; Code:
(in-package :cli/clap)

;;; Protocol
(defgeneric cmds (self))
(defgeneric opts (self))

(defgeneric cli-args (self)
  (:method ((self null)) (args)))

(defgeneric push-cmd (cmd place))

(defgeneric push-opt (opt place))

(defgeneric pop-cmd (place))

(defgeneric pop-opt (place))

(defgeneric find-cmd (name self &key active))

(defgeneric (setf find-cmd) (new name self))

(defgeneric find-opt (name self &key active default))

(defgeneric (setf find-opt) (new name self))

(defgeneric find-opts (name self &key active recurse))

(defgeneric active-cmds (self))

(defgeneric active-opts (self))

(defgeneric activate-opt (self))

(defgeneric activate-cmd (self))

(defgeneric call-opt (self arg))

(defgeneric do-opt (self))

(defgeneric do-opts (self))

(defgeneric call-cmd (self args opts))

(defgeneric do-cmd (self)
  (:documentation "Run the command SELF with args parsed at runtime."))

(defgeneric print-help (self &optional stream)
  (:documentation "Format cli SELF as a helpful string."))

(defgeneric print-version (self &optional stream)
  (:documentation "Print the version of SELF."))

(defgeneric print-usage (self &optional stream)
  (:documentation "Format cli SELF as a useful string."))

(defgeneric proc-args (self args))

(defgeneric parse-args (self args &key &allow-other-keys)
  (:documentation "Parse list of strings ARGS using SELF.

A list of the same length as ARGS is returned containing cli-node objects:
objects: (OPT . (or char string)) (CMD . string)"))

;;; Nodes
(defnode cli-node (ast)
    ((type :reader cli-node-type)))

(definline cli-node (type form) (make-instance 'cli-node :type type :ast form))
