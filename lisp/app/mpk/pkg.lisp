;;; pkg.lisp --- MPK Packages

;; 

;;; Commentary:

;; TODO: https://github.com/uways/oggify

;;; Code:
(defpackage :mpk
  (:use :cl :std :log :id :config :ast)
  (:export
   #:*mpk-directory*
   #:mpk-path
   :mpk-config :load-mpkrc
   #:mpk-ensure-directories
   #:*mpk-user-directory*
   #:*mpk-media-directory*
   #:*mpk-media-sources*))

(defpackage :mpk/db
  (:nicknames :mdb)
  (:use :cl :std :log :rdb :dsp/aud :dsp/gst :mpk :schema :db :id)
  (:export :*mdb-directory* :*mdb* :*mdb-schema*
   :mdb :init-mdb))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk :cli))
