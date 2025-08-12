;;; defsys.lisp --- defsystem extension macros

;; Intended to serve as a replacement for asdf:system utilities and quicklisp.

;;; Commentary:

;; goals:
;; - default to asdf (wrap)
;; - replace quicklisp (will need to be in lib/sys)
;; - share resources between system and dependency manager
;; - integrate with lib/packy (package distributor)
;; - multi-threaded by default
;; - parallel compilation (completely short-circuiting asdf)

;;; Code:
(in-package :std/defsys)
(declaim (optimize speed))
;;; Conditions
(define-condition defsys-condition () ())
(define-condition defsys-error (error defsys-condition) ())
(define-condition simple-defsys-error (simple-error defsys-condition) ())
(defun defsys-error (format &rest args)
  (error 'simple-defsys-error :format-control format :format-arguments args))

;;; Components
;;; Ops
;;; Actions
;;; Dependencies
;;; Systems
(defclass sysdef () ()
  (:documentation "System Definition"))

(defmacro defsys (name &body body)
  "Define a SYS with NAME and BODY interpreted similar to ASDF:DEFSYSTEM.

SYS objects register their own ASDF:SYSTEM objects as needed and provide the following extensions:

- :MODULES     list of system-provided modules
- :FEATURES    list of system-provided features
"
  `(defsystem ,name ,@body))

;;; Plan

;;; Modules
(defvar *module* nil)
(defparameter *core-module-table* (make-hash-table :test 'equal))

(defclass core-module () ())

(defun load-core-module (name)
  (let ((cmod (gethash name *core-module-table*)))
    (with-slots (load-hook exit-hook) cmod
      (when exit-hook
        (pushnew exit-hook sb-ext:*exit-hooks*))
      (funcall load-hook))))

(defmacro load-module (name)
  "Load module NAME from the global list *MODULES*."
  (let ((mod (find name *modules* :test 'string-equal)))
    (if (null mod) (warn "Module not found: ~A" name)
        (let ((core-mod (gethash mod *core-module-table*)))
           (if core-mod
               `(load-core-module ,core-mod)
               `(require ,mod))))))

(defun unload-module () (setf *module* nil))

(defun module-provide-core (name)
  "Provide a CORE-MODULE, adding valid entries to the *MODULES*
  variable. The function USE should be called in order to load and activate a
  module, but the deprecated PROVIDE function is also supported."
  (load-core-module name))

(defmacro with-module (name &body body)
  "Load the module named NAME, binding it to *MODULE* and eval BODY."
  `(let ((*module* (or (load-module ,name) ,name)))
     ,@body))

;; (with-eval-after-load (module &body body))
