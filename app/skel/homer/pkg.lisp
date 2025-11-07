;;; pkg.lisp --- HOMER Packages

;; 

;;; Code:
(defpackage :skel/homer/core
  (:use :cl :std :log :skel/krypt :skel/core :config :io/kbd :ast :id :time :pod :box :cli/tools/sys)
  (:import-from :srv :request :response :service :engine)
  ;; (:import-from :mpk :mpk-config :load-mpkrc)
  (:export
   #:*user-homedir*
   #:*user-homerc*
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
   #:homer-task
   #:*homer-logger*
   #:home-config
   :home-config-slot
   #:homer-job
   #:homer-service-start
   #:homer-service-restart
   #:systemd-service-config
   #:homer-service-config
   #:*systemd-config-directory*
   #:systemd-start
   #:systemd-restart
   #:systemd-stop
   #:systemd-status))

(defpackage :skel/homer/cli
  (:use :cl :std :log :skel/homer/core :cli :ast :clap)
  (:export :*homer-cli*))

(pkg:defpkg :skel/homer
  (:use :cl :std :log :cli)
  (:use-reexport :skel/homer/core :skel/homer/cli))
