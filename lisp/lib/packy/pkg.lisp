(defpackage :packy/core
  (:use :cl :std :rdb :obj :net :dat)
  (:export
   :pk-pack
   :pk-unpack
   :pk-install
   :pk-uninstall
   :pk-update
   :pk-push
   :pk-pull
   :pk-query
   :pk-sync
   :pk-build
   :pk-source
   :pk-package
   :pk-bundle
   :pk-dependency
   :pk-registry
   :pk-db
   :pk-config
   :pk-user-config
   :list-packages
   :push-package
   :pull-package
   :query-package
   :sync-package
   :update-package
   :build-package))

(defpackage :packy/client
  (:use :cl :std :packy/core))

(defpackage :packy/server
  (:use :cl :std :packy/core))

(defpackage :packy/pkgbuild
  (:use :cl :std))

(pkg:defpkg :packy
  (:nicknames :pk)
  (:use :cl :std)
  (:use-reexport :packy/client :packy/server :packy/core))

