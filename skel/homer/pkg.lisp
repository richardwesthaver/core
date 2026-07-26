;;; pkg.lisp --- HOMER Packages

;; 

;;; Code:
(defpkg :skel/homer/core
  (:use :cl :std :log :skel/krypt :skel/core :config :io/kbd :ast :id :time :pod :box :cli/tools/sys :project)
  (:import-from :srv :request :response :service :engine)
  ;; (:import-from :mpk :mpk-config :load-mpkrc)
  (:export
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
   #:systemd-service-config
   #:homer-service-config
   #:*systemd-config-directory*
   #:systemd-start
   #:systemd-restart
   #:systemd-stop
   #:systemd-status))

(defpkg :skel/homer/cli
  (:use :cl :std :log :skel/homer/core :cli :ast :clap :cmd :sb-ext))

(defpkg :skel/homer
  (:nicknames :homer)
  (:use :cl :std :log :cli)
  (:use-reexport :skel/homer/core))

(defpkg :homer-user
  (:use :std-lisp :log :sb-debug :sb-ext :ast :vc :rdb :uri)
  (:use :homer))
