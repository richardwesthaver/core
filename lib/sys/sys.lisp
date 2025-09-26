;;; sys.lisp --- SYS Core Compatibility

;; An extension to the STD/DEFSYS package which provides support for all
;; remaining core libraries.

;;; Commentary:

;; This file should be loaded right after the stage-1 libraries (see system
;; dependencies) and before the stage-2 dependencies (remaining modules in the
;; core).

;;; Code:
(in-package :std-user)

(defpkg :sys
  (:use :std-lisp)
  (:use-reexport :std/defsys))

(in-package :sys)
(in-readtable :core)

(defprovider :cli (name &key package)
  `(clap:load-package-cli ,name . ,(when package '(:package package))))

(defprovider :db (name)
  `(gethash ,name db:*database-backend-table*))

(defprovider :srv (name)
  `(gethash ,name srv:*service-table*))

;; (defprovider :pod (name))
;; (defprovider :box (name))

;; (defprovider :doc (name &rest args))

;; (defprovider :logger (name &rest args))
