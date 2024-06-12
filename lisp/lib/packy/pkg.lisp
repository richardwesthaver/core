;;; pkg.lisp --- Packy Packages

;; 

;;; Code:
(defpackage :packy/core
  (:use :cl :std :obj/id :dat/proto)
  (:export
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
   :package-version))

(defpackage :packy/db
  (:use :cl :std :packy/core :obj/db :rdb)
  (:export :package-database))

(defpackage :packy/client
  (:use :cl :std :packy/core :net/fetch))

(defpackage :packy/server
  (:use :cl :std :packy/core :net/srv))

(defpackage :packy/pkgbuild
  (:use :cl :std)
  (:export))

(pkg:defpkg :packy
  (:nicknames :pk)
  (:use :cl :std)
  (:use-reexport :packy/client :packy/server :packy/core))

