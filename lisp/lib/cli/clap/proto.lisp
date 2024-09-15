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
  (deferror clap-missing-argument (clap-error missing-argument)
      ((kind :initarg :kind :initform nil)))
  (deferror clap-invalid-argument (clap-error invalid-argument) ())
  (defwarning clap-simple-warning (simple-warning clap-warning) () (:auto t)))

(defun clap-unknown-argument (arg kind)
  (error 'clap-unknown-argument :name arg :kind kind))

(defun clap-missing-argument (arg kind)
  (error 'clap-missing-argument :item arg :kind kind))

(defun clap-invalid-argument (arg &key reason kind)
  (error 'clap-invalid-argument :name arg :kind kind :reason reason))

(defgeneric push-cmd (cmd place))

(defgeneric push-opt (opt place))

(defgeneric pop-cmd (place))

(defgeneric pop-opt (place))

(defgeneric find-cmd (self name &optional active))

(defgeneric find-opts (self name &key active recurse))

(defgeneric active-cmds (self))

(defgeneric active-opts (self &optional global))

(defgeneric activate-opt (self))

(defgeneric activate-cmd (self))

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

(defgeneric handle-unknown-opt (self opt)
  (:documentation "Handle an unknown cli-opt."))

(defgeneric handle-missing-opt (self opt)
  (:documentation "Handle a missing cli-opt."))

(defgeneric handle-invalid-opt (self opt &optional reason)
  (:documentation "Handle an invalid cli-opt."))

(defgeneric handle-unknown-arg (self arg)
  (:documentation "Handle an unknown cli-arg."))

(defgeneric handle-missing-arg (self arg)
  (:documentation "Handle a missing cli-arg."))

(defgeneric handle-invalid-arg (self arg &optional reason)
  (:documentation "Handle an invalid cli-arg."))

(defgeneric cli-equal (a b))
