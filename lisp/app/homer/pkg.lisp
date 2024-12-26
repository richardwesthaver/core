;;; pkg.lisp --- HOMER Packages

;; 

;;; Code:
(defpackage :homer/core
  (:use :cl :std :log :krypt :skel :config :io/kbd :ast :id)
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
   #:homer-maybe-install
   :homer-user-init
   #:homer-task))

#+cli
(defpackage :homer/cli
  (:use :cl :std :log :homer/core :cli :ast)
  (:export :*homer-cli*))

#+gui
(defpackage :homer/gui
  (:use :cl :std :log :homer/core :gui))

(pkg:defpkg :homer
  (:use :cl :std :log)
  (:use-reexport :homer/core #+cli :homer/cli #+gui :homer/gui))
