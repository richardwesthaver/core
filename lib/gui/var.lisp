;;; var.lisp --- GUI Variables

;; 

;;; Code:
(in-package :gui/core)

(defvar *default-application-class* nil
  "The current default application class (a symbol). Used as a default by
application launchers such as GUI/CLIM/APP:RUN-APP.")
(defvar *application* nil
  "The currently running gui application or NIL.")
(defparameter *gui-backend-list* '(:gtk :tk :mcclim))
(defparameter *gui-backend* nil)
