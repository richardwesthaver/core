;;; secret.lisp --- Secret (concealed) Objects

;; Object wrapper intended to prevent leaking of sensitive data.

;;; Commentary:

;; ref: https://github.com/rotatef/secret-values

;;; Code:
(in-package :obj/secret)

(defclass secret ()
  ((name :initform (symbol-name #1=(gensym "secret")) :type string :accessor secret-name :initarg :name)
   (symbol :initform #1# :type symbol :accessor secret-symbol :initarg :symbol))
  (:documentation "A 'secret' object which is hidden from view when printing to avoid embarassing
leakage of sensitive data."))

(defmethod print-object ((self secret) stream)
  (if (secret-name self)
      (print-unreadable-object (self stream :type t :identity t)
        (princ (secret-name self) stream))
      (print-unreadable-object (self stream :type t :identity t))))

(defgeneric conceal (self &key name class &allow-other-keys)
  (:documentation"Conceals value into a SECRET object. An optional name can be
provided to aid debugging.")
  (:method ((self t) &key name (class 'secret))
    (let ((secret (apply #'make-instance class `(,@(when name `(:name ,name))
                                                   ,@(when name `(:symbol ,(make-symbol name)))))))
      (setf (get (secret-symbol secret) class) (lambda () self))
      secret)))

(defgeneric reveal (self)
  (:documentation "Returns the secret value of SELF.")
  (:method ((self secret))
    (funcall (get (secret-symbol self) 'secret))))

(defgeneric ensure-concealed (object &key name &allow-other-keys)
  (:documentation "If object is already a of type SECRET-VALUE returns is unaltered,
  otherwise conceals it as if by calling CONCEAL-VALUE.")
  (:method ((self t) &key name)
    (typecase self
      (secret self)
      (t (conceal self :name name)))))

(defgeneric ensure-revealed (object)
  (:documentation "If object is type SECRET-VALUE returns the concealed value, otherwise returns object.")
  (:method ((self t))
    (typecase self
    (secret (reveal self))
    (t self))))
