;;; pkg.lisp --- Packy Packages

;; 

;;; Code:
(defpackage :skel/packy
  (:use :cl :std :id :dat/proto :io :ast :log :config :db :rdb :schema :time :net/core :net/srv)
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
   :*default-packy-dist-targets*
   :package-database
   :init-packy-db
   :package-id))

(defpackage :skel/packy/pkgbuild
  (:use :cl :std :ast :syn/ts :tree-sitter)
  (:export))

(defpackage :skel/packy/apkbuild
  (:use :cl :std :ast :syn/ts :tree-sitter)
  (:export))

(defpackage :skel/packy/cli
  (:use :cl :std :cli :clap :skel/packy)
  (:export :*packy-cli*))
