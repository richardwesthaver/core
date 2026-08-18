;;; obj.lisp --- OBJ Top-level

;; 

;;; Code:
(in-package :std-user)

(defpkg :obj
  (:use :cl :std)
  (:use-reexport . #.obj-int::*obj-packages*))

(in-package :obj)
;;; DEFSYS Providers
;; (defprovider :ast (name))
;; (defprovider :schema (name))
;; (defprovider :secret (name))
;; (defprovider :store (name))
;; (defprovider :project (name))
;; (defprovider :style (name))
;; (defprovider :palette (name))
;; (defprovider :config (name &rest args))

(defprovider :srv (name)
  `(gethash ,name srv:*service-table*))
