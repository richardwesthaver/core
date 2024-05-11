(in-package :skel/inspect)

(defgeneric sk-inspect (self &key)
  (:documentation "Open a skel object in the clouseau inspector."))

(defmethod sk-inspect ((self sk-project) &key (wait t))
  (sb-thread:make-thread #'clouseau:inspect :name "SK-INSPECTOR" :arguments (list self)))

