;;; pkg.lisp --- HOMER Packages

;; 

;;; Code:
(defpackage :homer
  (:use :cl :std :log :krypt :skel :config :io/kbd)
  (:import-from :mpk :mpk-config :load-mpkrc)
  (:export
   #:*user*
   #:*user-homedir*
   #:*default-user-homerc*
   #:*home-config*
   #:*home-hidden-paths*
   #:*homer-force*
   #:load-homerc
   #:compare-home-file
   #:homer-status
   #:homer-copy
   #:homer-maybe-push
   #:homer-maybe-pull
   #:homer-maybe-install))

#+cli
(defpackage :homer/cli
  (:use :cl :std :log :homer :cli))

#+gui
(defpackage :homer/gui
  (:use :cl :std :log :homer :gui))
