;;; pack.lisp --- Pack Objects

;; 

;;; Code:
(in-package :skel/core/obj)

;;; Pack
(declaim (inline %make-sk-pack))
(defstruct (sk-pack (:constructor %make-sk-pack))
  "Skel Package Object"
  (name (gensym "PKG"))
  (version 0 :type fixnum :read-only t)
  (sys nil :type (or null sys)))

(defmethod id ((self sk-pack))
  (sxhash (cons (sk-pack-name self) (sk-pack-version self))))

(defmethod sk-new ((self (eql :pack)) &rest args)
  (apply 'sk-new 'sk-pack args))
