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
  (:documentation "Load a configuration."))
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
  "DEFCLASS sugar for CONFIG objects."
  `(eval-always
     (defclass ,name ,(append direct-superclasses '(obj/config::config))
       ,direct-slots
       ,@options)))

;;; TODO 2024-10-27: Simple Config AST
(defmacro define-simple-config (name prototype &body accessors)
  "Define a SIMPLE-CONFIG consisting of a MAKE-* function, a predicate, a type
definition, and an optional list of accessors.")
