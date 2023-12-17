(defpackage :gui/core
  (:use :cl :std :log)
  (:export
   :gui-error))

(defpackage :gui/wm
  (:use :cl :std :log :gui/core :wayflan-client)
  (:export))

(defpackage :gui/proto/keyboard
  (:use :cl :std :gui/core :gui/wm)
  (:export))

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
