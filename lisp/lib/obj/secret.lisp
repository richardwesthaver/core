;;; secret.lisp --- Secret (concealed) Objects

;; Object wrapper intended to prevent leaking of sensitive data.

;;; Commentary:

;; ref: https://github.com/rotatef/secret-values

;;; Code:
(in-package :obj/secret)

(defclass secret-object ()
  ((name :initform (symbol-name #1=(gensym "secret")) :type string :accessor secret-object-name :initarg :name)
   (symbol :initform #1# :type symbol :accessor secret-object-symbol :initarg :symbol))
  (:documentation "A 'secret' object which is hidden from view when printing to avoid embarassing
leakage of sensitive data."))

(defmethod print-object ((self secret-object) stream)
  (if (secret-object-name self)
      (print-unreadable-object (self stream :type t :identity t)
        (princ (secret-object-name self) stream))
      (print-unreadable-object (self stream :type t :identity t))))

(defgeneric conceal-object (self &key name &allow-other-keys)
  (:documentation"Conceals value into a SECRET object. An optional name can be
provided to aid debugging.")
  (:method ((self t) &key name)
    (let ((secret (apply #'make-instance 'secret-object `(,@(when name `(:name ,name))
                                                          ,@(when name `(:symbol ,(make-symbol name)))))))
      (setf (get (secret-object-symbol secret) 'secret) (lambda () self))
      secret)))


(defgeneric reveal-object (self)
  (:documentation "Returns the secret value of SELF. An error of type TYPE-ERROR is
 signalled if the argument is not of type SECRET-OBJECT.")
  (:method ((self secret-object))
    (funcall (get (secret-object-symbol self) 'secret))))


(defgeneric ensure-concealed (object &key name &allow-other-keys)
  (:documentation "If object is already a of type SECRET-VALUE returns is unaltered,
  otherwise conceals it as if by calling CONCEAL-VALUE.")
  (:method ((self t) &key name)
    (typecase self
      (secret-object self)
      (t (conceal-object self :name name)))))

(defgeneric ensure-revealed (object)
  (:documentation "If object is type SECRET-VALUE returns the concealed value, otherwise returns object.")
  (:method ((self t))
    (typecase self
    (secret-object (reveal-object self))
    (t self))))
