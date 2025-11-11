(defpackage :skel/core/proto
  (:use :cl :std)
  (:export
   ;; generics
   :sk-run :sk-new 
   :sk-tangle :sk-weave
   :sk-call
   :sk-load :sk-save
   :sk-print :sk-read
   :sk-build
   :sk-compile :sk-transform
   :sk-write :sk-writeln
   :sk-write-string :sk-write-file
   :sk-read-file :sk-install-user-config
   :sk-find :sk-find*
   :sk-convert :sk-load-component
   :sk-install
   :sk-test
   :sk-deploy
   :sk-fetch
   :sk-pack
   :sk-unpack
   :sk-bundle
   :sk-unbundle
   :sk-register))

(defpackage :skel/core/int
  (:use :cl :std/condition :obj/ast :skel/core/proto)
  (:import-from :std/macs :eval-always)
  (:import-from :sb-unix :uid-username :unix-getuid)
  (:import-from :ast :*keep-ast*)
  (:import-from :std/path :merge-homedir-pathnames)
  (:import-from :vc :vc-designator)
  ;; conditions
  (:export
   :skel-condition
   :skel-error
   :skel-simple-error
   :skel-syntax-error
   :skel-io-error
   :skel-compile-error)
  ;; vars
  (:export
   :*skel-project* :*default-skelrc*
   :*skel-env* :*skel-project*
   :*default-skelfile* :*default-skel-user* 
   :*default-skel-vc-kind*
   :*default-skel-cache* :*skelfile-extension* :*skelfile-boundary*
   :*skel-hook*
   #:*skel-stash*
   #:*skel-store*
   #:*skel-cache*
   #:*skel-path*
   #:*system-skelrc*
   #:*user-skelrc*
   #:user-skelrc
   #:*skel-data*
   #:*default-skel-bindings*
   #:*skel-project-functions*
   #:*skel-project-symbol-macros*
   #:*skel-project-macros*))

(defpackage :skel/core/header
  (:use :cl :std :skel/core/int :doc)
  (:export
   :make-file-header 
   :make-shebang-file-header 
   :make-source-file-header 
   :file-header-kind
   :file-header
   :make-source-header-comment 
   :make-shebang-comment))

(defpackage :skel/core/obj
  (:use :cl :std :obj
        :skel/core/proto :skel/core/int
        :skel/core/header :vc :log :config :id :ast :project)
  (:import-from :uiop :ensure-absolute-pathname :read-file-forms)
  (:export :sk-stash :sk-data :user
   :sk-push :sk-pull
   :edit-skelrc :sk-target :skel
   :sk-meta :def-sk-class :sk-project :sk-source
   :sk-env :make-sk-rule
   :sk-rule :sk-rule-target :sk-rule-source :sk-rule-recipe
   :sk-make :sk-kind
   :sk-command :scripts :sk-script :sk-config
   :sk-snippet :sk-abbrev
   :sk-user-config :sk-system-config
   :*skel-user-config* :*skel-system-config*
   :sk-component :sk-mod
   :sk-parent :skel-store :with-skel-ast :sk-pack))

(defpackage :skel/core/schema
  (:use :cl :std :skel/core/obj :skel/core/proto :skel/core/int :schema :rdb)
  (:export :sk-object-schema :sk-schema :*skel-registry-schema* :*skel-cache-schema*))

(defpackage :skel/core/db
  (:use :cl :std :skel/core/int :skel/core/schema
        :skel/core/obj :skel/core/proto :db
        :store :schema :rdb
        :id :stored :log :config :build)
  (:export :skel-db 
   :skel-db-path))

(defpackage :skel/core/log
  (:use :cl :std :log :skel/core/db :skel/core/int 
        :skel/core/obj :skel/core/proto :db
        :store :schema :rdb :id :stored :log :config :build :skel/core/schema)
  (:export
   :sk-log-schema
   :*skel-log-schema*
   :skel-db-logger
   :*skel-logger-config*
   :*skel-logger*
   :init-skel-logger
   :sk-log-shutdown
   :sk-log-repair
   :sk-log-close
   :sk-log-list
   :skel-db-sink))

(defpackage :skel/core/util
  (:use :cl :std :skel/core/obj :skel/core/proto :skel/core/int :obj/ast)
  (:import-from :uiop/pathname :pathname-parent-directory-pathname)
  (:import-from :cli :find-exe)
  (:export
   :init-skelrc :load-skelrc
   :init-skel
   :init-user-skelrc :load-user-skelrc
   :init-system-skelrc :load-system-skelrc
   :init-skelfile
   :load-skelfile
   :find-skelfile
   :find-sk-file
   :sk-config-slot
   :sk-project-slot
   :find-project-root
   :setf-skel-vars
   :list-all-projects
   :sk-search-project
   :project-root))


(defpackage :skel/core/print
  (:use :cl :std :skel/core/int :skel/core/obj :skel/core/proto)
  (:export))

(defpackage :skel/core/plan
  (:use :cl :std :skel/core/int :skel/core/obj :skel/core/proto))
