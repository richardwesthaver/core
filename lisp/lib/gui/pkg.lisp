(defpackage :gui/core
  (:use :cl :std :log)
  (:export
   :gui-error
   :gui-client-p :gui-server-p))

(defpackage :gui/wm
  (:use :cl :std :log :gui/core #+wl :wayflan)
  (:export
   :*default-wm*
   :wm-package))

(defpackage :gui/ext
  (:use :cl :std :log :gui/core)
  (:export 
   :*gui-backend-list*
   :*gui-backend*
   :register-gui-backend
   :load-gui-backend
   :with-gui-handlers
   :gui-main
   :define-gui))
  
(uiop:define-package :gui
  (:use-reexport :gui/core :gui/wm :gui/ext))
