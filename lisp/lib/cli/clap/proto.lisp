;;; cli/clap/proto.lisp --- Clap Protocol

;; 

;;; Code:
(in-package :cli/clap/proto)

(define-condition clap-condition () ())
(eval-always
  (deferror clap-error (clap-condition) ())
  (defwarning clap-warning (clap-condition) ())
  (deferror clap-simple-error (simple-error clap-error) () (:auto t))
  (deferror clap-unknown-argument (clap-error unknown-argument) ())
  (defwarning clap-simple-warning (simple-warning clap-warning) () (:auto t)))

(defun clap-unknown-argument (opt)
  (error 'clap-unknown-argument :name opt :kind 'cli-opt))

(defgeneric push-cmd (cmd place))

(defgeneric push-opt (opt place))

(defgeneric pop-cmd (place))

(defgeneric pop-opt (place))

(defgeneric find-cmd (self name &optional active))

(defgeneric find-opts (self name &key active recurse))

(defgeneric active-cmds (self))

(defgeneric active-opts (self &optional global))

(defgeneric find-short-opts (self ch &key))

(defgeneric call-opt (self arg))

(defgeneric do-opt (self))

(defgeneric do-opts (self &optional global))

(defgeneric call-cmd (self args opts))

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
