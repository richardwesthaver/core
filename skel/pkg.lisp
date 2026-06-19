;;; skel/pkg.lisp --- Project Skeletons

;; Project composition library.

;;; Commentary:

;; The SKEL system consists of a core package SKEL/CORE, a required compiler
;; package SKEL/COMP, and some default but optional modules SKEL/DB and
;; SKEL/NET. 

;; The core contains all of the low-level bits and a CLOS API for interacting
;; with SKEL objects. 

;; The compiler package depends on the core and provides primitive compilers
;; for translating SKEL objects into foreign formats. For example, RULE
;; objects may be translated into a corresponding GNU Make Rule. The compiler
;; packages implement the CLOS API defined in the core and may serve as a
;; useful guide for further extending the system yourself.

;; SKEL/DB provides a database interface for individual project caches as well
;; as global storage. SKEL/NET provides a wire protocol and client/server for
;; communication amongst any number of remote hosts.

;; Additionally there is a collection of default extensions which may be
;; toggled off via the SKEL-CONFIG FEATURES slot:

;; - VIEW provides an API for generating visualizations of SKEL objects

;; - PACKY enables package management and distribution.

;; - POD enables Podman API functionality.

;; - BOX enables QEMU/libvirt features.

;; - DEPLOY enables CI/Deploy features.

;;; Code:
(defpkg :skel/core
  (:use :std-lisp :ast :doc :log :config :project :schema :rdb :db :store :stored :id :vc)
  (:import-from :sb-unix :uid-username :unix-getuid)
  (:import-from :cli :find-exe)
  (:export
   ;; conditions
   :skel-condition
   :skel-error
   :skel-simple-error
   :skel-syntax-error
   :skel-io-error
   :skel-compile-error
   ;; vars
   :*default-skelrc*
   :*skel-project*
   :*default-skelfile* :*default-skel-user* 
   :*default-skel-cache* :*skelfile-extension* :*skelfile-boundary*
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
   :edit-skelrc :skel
   :def-sk-class :skel-project
   :sk-env
   :make :sk-kind
   :sk-command :scripts :project-script :skel-config
   :skel-user-config :skel-system-config
   :*skel-user-config* :*skel-system-config*
   :skel-store
   :print-skel-object
   :*skel-slot-exclusions*
   ;; schema
   :sk-object-schema :skel-schema :*skel-registry-schema* :*skel-cache-schema*
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
   :setf-skel-vars
   :project-root
   :merge-project-pathnames))

(defpkg :skel/comp/make
  (:use :std-lisp :skel/core :project)
  (:export
   :*default-makefile* :*makefile-extension* 
   :*mk-magic-vars* :*mk-command-prefixes*
   :push-mk-rule :push-mk-var :push-mk-directive
   :mk-val-designator 
   :mk-val :mk-var
   :makefile))

(defpkg :skel/comp/shell
  (:use :std-lisp :skel/core :ast :syn/ts :project)
  (:export :pkgbuild))

(defpkg :skel/comp/rust
  (:use :std-lisp :skel/core :toml :config :cli/tools/build :project)
  (:export :rust-system :parse-rust-system))

(defpkg :skel/comp/python
  (:use :std-lisp :skel/core :toml :config :cli/tools/build :project)
  (:export :python-system :parse-python-system))

(defpkg :skel/comp/lisp
  (:shadowing-import-from :std :version)
  (:use :std-lisp :skel/core :id :project)
  (:import-from :ast :ast :read-ast :write-ast :load-ast)
  (:export :lisp-file :lisp-component :lisp-system 
   :read-system-definitions :parse-lisp-system :write-asd-components))

(defpkg :skel/comp/pod
  (:use :cl :std :pod :skel/core :id :ast :project)
  (:export :project-containerfile))

(defpkg :skel/comp/emacs
  (:use :cl :std :skel/core :ast :id :organ :project)
  (:export :*dir-locals-file* :dir-local-var-designator :dir-locals :emacs-component :emacs-lisp-file
   :org-file))

(defpkg :skel/comp/box
  (:use :cl :std :skel/core :box :id :project)
  (:export :box-file))

(defpkg :skel/comp/infer
  (:use :cl :std :skel/core :srv :id :ast :dat :config :vc :doc :project)
  (:export :project-infer))

(defpkg :skel/comp
  (:nicknames :sk-comp)
  (:use :cl :std)
  (:use-reexport :skel/comp/rust :skel/comp/make :skel/comp/python
   :skel/comp/pod :skel/comp/emacs :skel/comp/box :skel/comp/infer
   :skel/comp/shell :skel/comp/lisp))

(defpkg :skel/cli
  (:nicknames :sk-cli)
  (:use :cl :std :log :skel/core :sb-ext :clap :cli/main :project))

(defpkg :skel/srv
  (:use :cl :std :db 
   :store :config :skel/core
   :net/srv/udp :net/srv/http :srv :project)
  (:export #:skel-service
           #:skel-request
           #:skel-response))

(defpkg :skel/net
  (:nicknames :sk-net)
  (:use :cl :std :net/srv/udp :skel/core :srv :log :skel/srv))

(defpkg :skel
  (:nicknames :sk)
  (:use :cl :std)
  (:use-reexport 
   :skel/core :skel/comp 
   :skel/net :project))

(defpkg :sk-user
  (:use :std-lisp :cli :tools
    :cl-user :log :sb-debug :sb-ext
    :obj/ast :vc :rdb :io :net :pod :uri
    :project)
  (:use :skel))

(init :commands :name :skel :copy :cli :clean t)
