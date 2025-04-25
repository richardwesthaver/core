;;; proto.lisp --- GUI Protocol

;; 

;;; Code:
(in-package :gui/core)

#|
(defapp () )
|#

(defun register-gui-backend (name &optional lib)
  "Register a new GUI backend named NAME. if LIB is provided it is
assumed to be a path to a shared library.")

(defun load-gui-backend (backend)
  (case backend
    (:gtk (nyi!))
    (:tk (nyi!))
    (:mcclim (ql:quickload :mcclim))
    (:slint (nyi!))))

(defmacro with-gui-handlers (&body body)
  `(progn
     ,@body))

(defmacro define-gui (ret &body body)
  `(progn
     (declaim (type stream output))
     (defun gui-main (&key (output *standard-output*))
       "Run the top-level function and print to OUTPUT."
       (let ((*standard-output* output))
         (with-gui-handlers
             (progn ,@body ,ret))))))
