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
(defparameter *core-modules* nil)

(defun load-module (name)
  "Load module NAME from the global list *MODULES*."
  (find name *modules* :test 'string-equal))

(defun unload-module () (setf *module* nil))
  
(defun module-provide-core (name)
  "Provide a CORE-MODULE, adding valid entries to the *MODULES*
  variable. The function USE should be called in order to load and activate a
  module, but the deprecated PROVIDE function is also supported."
  (or (module-provide-asdf name)
      (module-provide-contrib name)))

(defmacro with-module (name &body body)
  "Load the module named NAME, binding it to *MODULE* and eval BODY."
  `(let ((*module* (load-module ,name)))
     ,@body))
