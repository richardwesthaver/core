;;; pkg.lisp --- Packy Packages

;; 

;;; Code:
(defpackage :packy/core
  (:use :cl :std :obj/id :dat/proto :io)
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
   :*packy-decompressor*))

(defpackage :packy/db
  (:use :cl :std :packy/core :obj/db :rdb)
  (:export :package-database))

(defpackage :packy/client
  (:use :cl :std :packy/core :net/core)
  (:export :pk-index
           :packy-client))

(defpackage :packy/server
  (:use :cl :std :packy/core :net/srv)
  (:export
   #:packy-service
   #:packy-server))

(defpackage :packy/pkgbuild
  (:use :cl :std)
  (:export))

(defpackage :packy/cli
  (:use :cl :std :cli :packy/client :packy/server :packy/db :packy/core)
  (:export :*packy-cli*))
