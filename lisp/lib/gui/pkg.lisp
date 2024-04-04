(defpackage :gui/core
  (:use :cl :std :log)
  (:export
   :gui-error
   :gui-client-p :gui-server-p))

(defpackage :gui/wm
  (:use :cl :std :log :gui/core :wayflan)
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
   :def-gui))

(defpackage :gui/slint
  (:use :cl :std :log :gui/core :gui/ext :parse) ;; yacc or lex
  (:export :compile-slint :compile-to-slint-file :compile-to-slint-string
   :*slint-grammar* :with-slint))
  
(uiop:define-package :gui
  (:use-reexport :gui/core :gui/wm :gui/ext))
