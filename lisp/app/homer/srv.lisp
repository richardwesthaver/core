;;; srv.lisp --- Homer Services

;; 

;;; Code:
(in-package :homer/core)

(defclass homer-service (id ast) ()
  (:documentation "Base class for HOMER services. Services are similar to Systemd units - they
may be individually controlled by an ORACLE thread (usually the default
toplevel)."))

(defmethod name ((self homer-service)) (id self))
