;;; obj/config.lisp --- Configuration flavors

;;

;;; Commentary:
;; The goal of this package is to make it easy to map an object in
;; memory to a 'user config interface' - which could be a
;; configuration file, a datagram, CLI flags, etc.
;;
;; This package only provides the config protocol, for other packages to
;; consume.

;;; Usage: 
#|
|#
;;; Code:
(in-package :obj/config)

(defclass config () ()
  (:documentation "Base class for configurable objects."))
(defgeneric make-config (obj &rest args &key &allow-other-keys)
  (:documentation "Make a new configuration.")
  (:method ((self t) &key ast)
    (typecase ast
      (list (make-config (car ast) :path (cadr ast)))
      (atom (make-config ast)))))
(defgeneric load-config (kind from &key &allow-other-keys)
  (:documentation "Load a configuration.")
  (:method ((self t) (from t) &rest args) (load-ast (apply 'read-ast self from args))))
(defgeneric find-config (obj &rest args &key &allow-other-keys)
  (:documentation "Find an existing configuration."))
(defgeneric config-find (obj key &key &allow-other-keys)
  (:documentation "Find KEY in configuration OBJ."))
(defgeneric config-get (obj key)
  (:documentation "Get value of KEY in configuration OBJ."))
(defgeneric (setf config-get) (obj key val))
(defgeneric configure (obj &rest args &key &allow-other-keys)
  (:documentation "Configure an object with supplied args."))

(defmacro defconfig (name direct-superclasses direct-slots &rest options)
  "DEFCLASS sugar for CONFIG objects. INITARGS are automatically populated based
on the slot name."
  `(progn
     (defclass ,name ,(append direct-superclasses '(obj/config::config))
       ,(mapcar (lambda (x) 
                  (if (atom x) 
                      `(,x :initarg ,(keywordicate x))
                      (destructuring-bind (sym &rest rest) x
                        (if (getf rest :initarg)
                            x
                            `(,sym ,@rest :initarg ,(keywordicate sym))))))
         direct-slots)
       ,@options)))

;;; TODO 2024-10-27: Simple Config AST
(defmacro define-simple-config (name prototype &body accessors)
  "Define a SIMPLE-CONFIG consisting of a MAKE-* function, a predicate, a type
definition, and an optional list of accessors."
  `(progn
     (defconfig ,name (ast) ,prototype)))

;;; Late IO Config Classes
(defconfig io/kbd:kbd-config ()
  ((io/kbd::device)
   (io/kbd:prefix-key :initform (io/kbd:kbd "s-x") :accessor io/kbd:prefix-key)
   (io/kbd:escape-key :initform (io/kbd:kbd "C-g") :accessor io/kbd:escape-key)
   (io/kbd:keymaps :initform nil :accessor io/kbd:keymaps)))

(defmethod make-config ((self (eql :kbd)) &rest args) (apply 'make-instance 'io/kbd:kbd-config args))

(defmethod load-ast ((self io/kbd:kbd-config))
  (with-slots (ast) self
    (sb-int:doplist (k v) ast
      (when-let ((s (find-symbol (format nil "~A" k) :io/kbd)))
        (unless (null v)
          (setf v
                (case k
                  ((or :escape-key :prefix-key) (io/kbd:parse-key v))
                  (t v)))
          (setf (slot-value self s) v))))
      (unless *keep-ast* (setf (ast self) nil))))

(defmethod load-config ((self (eql :kbd)) (from pathname) &key)
  (let ((c (make-config :kbd)))
    (with-safe-io-syntax (:io/kbd)
      (read-ast c from)
      (load-ast c))
    (setf (ast c) nil)
    c))

(defmethod load-config ((self (eql :kbd)) (from list) &key)
  (let ((c (make-config :kbd)))
    (sb-int:doplist (k v) from
      (when-let ((s (find-symbol (format nil "~A" k) :io/kbd)))
        (unless (null v)
          (case k
            ((or :escape-key :prefix-key) (setf (slot-value c s) (io/kbd:parse-key v)))))))
    c))
