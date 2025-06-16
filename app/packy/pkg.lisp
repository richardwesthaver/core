;;; pkg.lisp --- Packy Packages

;; 

;;; Code:
(defpackage :packy/core
  (:use :cl :std :id :dat/proto :io :ast :log :config)
  (:export
   :*packy-url*
   :pack
   :unpack
   :install-package
   :uninstall-package
   :update-package
   :push-package
   :pull-package
   :query-package
   :sync-package
   :build-package
   :package-source
   :bundle-package
   :package-dependency
   :package-registry
   :packy-config
   :packy-user-config
   :list-packages
   :prepare-package
   :check-package
   :package-version
   :*pack*
   :*packy-registry*
   :*packy-compressor*
   :*packy-decompressor*
   :*packy-db*
   :*packy-logger*
   :*packy-home*
   :packy-condition
   :packy-error
   :*packy-dist-targets*
   :*default-packy-dist-targets*))

(defpackage :packy/db
  (:use :cl :std :packy/core :db :rdb :schema :time)
  (:export :package-database
           :init-packy-db))

(defpackage :packy/client
  (:use :cl :std :packy/core :net/core)
  (:export :pk-index
           :packy-client
           :init-packy))

(defpackage :packy/server
  (:use :cl :std :packy/core :net/srv)
  (:export
   #:packy-service
   #:packy-server))

(defpackage :packy/pkgbuild
  (:use :cl :std :ast)
  (:export))

(defpackage :packy/apkbuild
  (:use :cl :std :ast)
  (:export))

(defpackage :packy/cli
  (:use :cl :std :cli :packy/client :packy/server :packy/db :packy/core)
  (:export :*packy-cli*))

(in-package :packy/core)
