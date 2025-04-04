;;; pkg.lisp --- MPK Packages

;; 

;;; Commentary:

;; TODO: https://github.com/uways/oggify

;; ref: https://github.com/schismtracker/schismtracker/wiki/ITTECH.TXT

;;; Code:
(defpackage :mpk
  (:use :cl :std :log :id :config :ast :cli/tools/net :cli/tools/media :time)
  (:export
   #:*mpk-directory*
   #:mpk-path
   :mpk-config :load-mpkrc
   #:mpk-ensure-directories
   #:*mpk-user-directory*
   #:*mpk-media-directory*
   #:*mpk-media-sources*
   #:*known-media-types*
   #:*mpk-media-types*
   #:mpk-previous
   #:mpk-shuffle
   #:mpk-stop
   #:mpk-pause
   #:mpk-play
   #:*mpk-data-directory*
   #:*mpk-cache-directory*
   #:mpk-media-collection
   #:*mpk-media-collections*
   #:mpk-user-path
   #:mpk-media-path
   #:mpk-music-path
   #:mpk-data-path))

(defpackage :mpk/db
  (:nicknames :mdb)
  (:use :cl :std :log :rdb :dsp/aud :dsp/gst :mpk :schema :db :id :uuid)
  (:export :*mdb-directory* :*mdb* :*mdb-schema*
   :mdb :init-mdb))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk :cli)
  (:export
   #:*mpk-cli*))
