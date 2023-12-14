(uiop:define-package :gui
  (:nicknames :gui)
  (:use :cl :std :log)
  (:export 
   :*gui-backend-list*
   :*gui-backend*
   :register-gui-backend
   :load-gui-backend
   :with-gui-handlers
   :gui-main
   :def-gui))

(in-package :gui)

(defparameter *gui-backend-list* '(:gtk :tk :mcclim))

(defparameter *gui-backend* nil)

(defun register-gui-backend (name &optional lib)
  "Register a new GUI backend named NAME. if LIB is provided it is
assumed to be a path to a shared library.")

(defun load-gui-backend (backend)
  (case backend
    (:gtk (nyi!))
    (:tk (nyi!))
    (:mcclim (nyi!))))

(defmacro with-gui-handlers (&body body)
  `(progn
     ,@body))

(defmacro def-gui (ret &body body)
  "Define a CLI main function in the current package which returns RET.

Note that this macro does not export the defined function and requires
`gui:main' to be an external symbol."
  `(progn
     (declaim (type stream output))
     (defun gui-main (&key (output *standard-output*))
       "Run the top-level function and print to OUTPUT."
       (let ((*standard-output* output))
	 (with-gui-handlers
	     (progn ,@body ,ret))))))
