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

;;; Systems
(defclass sysdef () ())

(defmacro defsys (name &body body)
  `(defsystem ,name ,@body))

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
