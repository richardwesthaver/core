;;; skel/pkg.lisp --- Project Skeletons

;; Project composition library.

;;; Commentary:

;; The SKEL system consists of a core package SKEL/CORE, a required compiler
;; package SKEL/COMP, and some default but optional modules SKEL/DB and
;; SKEL/NET. 

;; The core contains all of the low-level bits and a CLOS API for interacting
;; with SKEL objects. 

;; The compiler package depends on the core and provides primitive compilers
;; for translating SKEL objects into foreign formats. For example, SK-RULE
;; objects may be translated into a corresponding GNU Make Rule. The compiler
;; packages implement the CLOS API defined in the core and may serve as a
;; useful guide for further extending the system yourself.

;; SKEL/DB provides a database interface for individual project caches as well
;; as global storage. SKEL/NET provides a wire protocol and client/server for
;; communication amongst any number of remote hosts.

;; Additionally there is a collection of default extensions which may be
;; toggled off via the SK-CONFIG FEATURES slot:

;; - VIEW provides an API for generating visualizations of SKEL objects

;; - PACKY enables package management and distribution.

;; - POD enables Podman API functionality.

;; - BOX enables QEMU/libvirt features.

;; - DEPLOY enables CI/Deploy features.

;;; Code:
(in-package :std-user)

(defpackage :skel/core
  (:use :cl :std :ast :doc :log :config :project :schema :rdb :db :store :stored :build :id :vc)
  (:import-from :sb-unix :uid-username :unix-getuid)
  (:import-from :cli :find-exe)
  (:export
   ;; generics
   :sk-run :sk-new 
   :sk-call :sk-load
   :sk-build
   :sk-compile
   :sk-write
   :sk-write-file
   :sk-read-file
   :sk-find
   :sk-convert :sk-load-component
   ;; conditions
   :skel-condition
   :skel-error
   :skel-simple-error
   :skel-syntax-error
   :skel-io-error
   :skel-compile-error
   ;; vars
   :*skel-project* :*default-skelrc*
   :*skel-env* :*skel-project*
   :*default-skelfile* :*default-skel-user* 
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
   #:*skel-project-macros*
   #:*default-clean-function*
   ;; header
   :make-file-header 
   :make-shebang-file-header 
   :make-source-file-header 
   :file-header-kind
   :file-header
   :make-source-header-comment 
   :make-shebang-comment
   ;; objects
   :sk-stash :sk-data :user
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
   :sk-parent :skel-store :with-skel-ast :sk-pack
   :print-skel-object
   ;; schema
   :sk-object-schema :sk-schema :*skel-registry-schema* :*skel-cache-schema*
   ;; db
   :skel-db 
   :skel-db-path
   ;; log
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
   :skel-db-sink
   ;; util
   :load-skelrc
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
   :sk-search-project
   :project-root
   :merge-project-pathnames))

(defpackage :skel/comp/makefile
  (:use :cl :std :skel/core :project)
  (:export
   :*default-makefile* :*makefile-extension* 
   :*mk-magic-vars* :*mk-command-prefixes*
   :push-mk-rule :push-mk-var :push-mk-directive
   :mk-val-designator 
   :mk-val :mk-var
   :makefile))

(defpackage :skel/comp/cargo
  (:use :cl :std :skel/core :toml :build :config :cli/tools/rust)
  (:export :sk-rust-system :parse-sk-rust-system))

(defpackage :skel/comp/sys
  (:use :cl :skel/core :std/defsys))

(pkg:defpkg :skel/comp/asd
  (:shadowing-import-from :std :version)
  (:import-from :std :defmethods :when-let)
  (:use :cl :skel/core :std/macs :asdf)
  (:import-from :asdf :system :coerce-name 
   :system-source-file :parse-component-form :file-component :component-relative-pathname
   :component-if-feature :component-depends-on :module-components :component-name
   :component-version :system-depends-on :system-description :system-long-description
   :system-author :system-maintainer :system-mailto :system-license
   :system-homepage :system-bug-tracker :system-source-control :component-in-order-to
   :component-build-pathname :component-build-operation :component-entry-point)
  (:export :sk-lisp-system :read-system-definitions :parse-sk-lisp-system :sk-write-asd-components))

(defpackage :skel/comp/lisp
  (:import-from :skel/core :*skel-project*)
  (:shadowing-import-from :std :version)
  (:use :cl :std :skel/core :id)
  (:import-from :ast :ast :read-ast :write-ast :load-ast)
  (:export :sk-lisp-file))

(defpackage :skel/comp/container
  (:use :cl :std :pod :skel/core :dat/proto :obj/id)
  (:export :sk-containerfile))

(defpackage :skel/comp/dir-locals
  (:use :cl :std :skel/core)
  (:export :*dir-locals-file* :dir-local-var-designator :sk-dir-locals))

(defpackage :skel/comp/org
  (:use :cl :std :skel/core :organ :obj/id)
  (:export :sk-org-file))

(defpackage :skel/comp/box
  (:use :cl :std :skel/core :box :obj/id)
  (:export :sk-box-file))

(defpackage :skel/comp/infer
  (:use :cl :std :skel/core :srv :id :ast :dat :config :vc :nlp)
  (:export :sk-infer))

(defpkg :skel/comp
  (:nicknames :sk-comp)
  (:use :cl :std)
  (:use-reexport :skel/comp/asd :skel/comp/cargo :skel/comp/makefile
   :skel/comp/container :skel/comp/dir-locals :skel/comp/org))

(defpkg :skel/cli
  (:nicknames :sk-cli)
  (:use :cl :std :log :skel/core :sb-ext :clap :cli/main))

(defpackage :skel/net/core
  (:nicknames :sk-net-core)
  (:use :cl :log :std 
   :net/core :net/proto/dns :net/codec/tlv :skel/core
   :skel/core :net/udp :net/tcp :obj/id 
   :net/srv/udp
   :dat/proto :dat/json)
  (:export
   #:*skel-client-port-range*
   #:*skel-port*
   #:*skel-service-port*
   #:*default-skel-service-port*))

(defpackage :skel/srv
  (:use :cl :std :db 
   :store :build :config :skel/core
   :net/srv/udp :net/srv/http :srv)
  (:export #:sk-service
           #:sk-request
           #:sk-response))

(defpackage :skel/net/client
  (:nicknames :sk-client)
  (:use :cl :std :net :skel/net/core :net/srv/udp :srv :skel/core :log :skel/srv)
  (:export))

(defpackage :skel/net/server
  (:nicknames :sk-server)
  (:use :cl :std :net/srv/udp :net/srv/http :sk-net-core :log :skel/core :srv :skel/srv)
  (:export :sk-server))

(defpkg :skel/net
  (:nicknames :sk-net)
  (:use :cl :std :net/srv/udp :skel/core :srv :log :skel/srv)
  (:use-reexport :skel/net/client :skel/net/server))

(pkg:defpkg :skel
  (:nicknames :sk)
  (:use :cl :std)
  (:use-reexport 
   :skel/core :skel/comp 
   :skel/net)
  (:export :with-project))

(pkg:defpkg :sk-user
  (:use :cl :std :cli :clap :tools
   :cl-user :log :sb-debug :sb-ext
   :net/proto/dns :obj/ast :vc :rdb 
   :io :net :pod)
  (:import-from :uri :uri)
  (:use :skel :skel/core :skel/comp :skel/net))

