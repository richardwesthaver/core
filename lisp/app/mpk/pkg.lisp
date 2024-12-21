;;; pkg.lisp --- MPK Packages

;; 

;;; Code:
(defpackage :mpk/int
  (:use :cl :std :log :id :config)
  (:export
   #:*mpk-directory*
   #:mpk-path
   #:mpk-ensure-directories
   #:*mpk-user-directory*
   #:*mpk-media-directory*
   #:*mpk-media-sources*))

(defpackage :mpk/db
  (:nicknames :mdb)
  (:use :cl :std :log :rdb :dsp/aud :dsp/gst :mpk/int :schema :db :id)
  (:export :*mdb-directory* :*mdb* :*mdb-schema*
   :mdb :init-mdb))

(defpackage :mpk
  (:use :cl :std :log :mpk/int))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk :cli))
