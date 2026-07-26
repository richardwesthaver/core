;;; pkg.lisp --- Packy Packages

;; 

;;; Code:
(defpkg :skel/packy
  (:nicknames :packy)
  (:use :cl :std :id :io :ast :log :config :db :rdb :schema :time :net/core :net/srv
    :skel :project)
  (:export
   :*packy-url*
   :unpack
   :pack
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
   :*machine-target-table*
   :package-database
   :init-packy-db
   :package-id
   :*default-pkgfile*
   :pkgfile
   :*default-pkg-extension*))

(defpkg :skel/packy/cli
  (:use :cl :std :clap :cmd :skel/packy :cli/main))

(defpkg :packy-user
  (:use :std-lisp :log :sb-debug :sb-ext :ast :vc :rdb :uri)
  (:use :packy))
