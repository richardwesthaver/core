(in-package :gui/core)

(define-condition gui-condition () ())

(deferror gui-error (gui-condition) () (:auto t))
(defwarning gui-warning (gui-condition) () (:auto t))
