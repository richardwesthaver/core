;;; pkg.lisp --- HOMER Packages

;; 

;;; Code:
(defpackage :homer/core
  (:use :cl :std :log :krypt :skel :config :io/kbd :ast :id :time :pod :box :cli/tools/sys)
  (:import-from :srv :request :response :service :engine)
  (:import-from :mpk :mpk-config :load-mpkrc)
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
   :homer-user-init
   #:homer-task
   #:*homer-task-pool*
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

(defpackage :homer/cli
  (:use :cl :std :log :homer/core :cli :ast :clap)
  (:export :*homer-cli*))

(pkg:defpkg :homer
  (:use :cl :std :log :cli)
  (:use-reexport :homer/core :homer/cli))
