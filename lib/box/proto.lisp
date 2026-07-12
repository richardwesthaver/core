;;; box/proto.lisp --- Box Protocols

;;

;;; Commentary:

;;; Code:
(in-package :box)

(defconfig box-config (ast) ()
  (:documentation "Base configuration class for BOX objects."))

(defmethod make-config ((self (eql :box)) &rest args &key (class 'box-config) ast from)
  (remf args :class)
  (remf args :from)
  (let ((cfg (if from (progn (setf (ast from) ast) from) (apply 'make-instance class args))))
    (when ast
      (load-ast cfg)
      (setf (ast cfg) nil))
    cfg))

(defmethod load-config ((fmt (eql :box)) from &key type build)
  (with-safe-io-syntax (:box)
    (let* ((ast (read-lisp-file from))
           (class (getf ast :class))
           (type (or type (getf ast :type)))
           (%from (getf ast :from))
           obj)
      (when %from (setf %from (load-config :box (make-pathname :name (namestring %from) :directory (pathname-directory from)))))
      (setf ast (remove-from-plist ast :class :type :from))
      (cond 
        ((and class type)
         (error 'invalid-argument :reason "invalid AST - CLASS and TYPE are incompatible options." :item type))
        (class (setf obj (make-config :box :class class :ast ast :from %from)))
        (type (setf obj (make-config :box :class (find-symbol (concatenate 'string (string-upcase type) "-CONFIG") :box) :ast ast :from %from)))
        (t (setf obj (make-config :box :ast ast :from %from))))
      (typecase build 
        (null)
        (list (apply 'build obj build))
        (t (build obj)))
      obj)))
