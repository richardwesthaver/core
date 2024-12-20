;;; pkg.lisp --- MPK Packages

;; 

;;; Code:
(defpackage :mpk/int
  (:use :cl :std :log)
  (:export
   #:*mpk-directory*
   #:mpk-path
   #:mpk-ensure-directories
   #:*mpk-user-directory*
   #:*mpk-media-directory*
   #:*mpk-media-sources*))

(defpackage :mdb
  (:use :cl :std :log :rdb :dsp/aud :dsp/gst :mpk/int :schema :db))

(defpackage :mpk
  (:use :cl :std :log :mpk/int))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk :cli))
