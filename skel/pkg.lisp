;;; skel/pkg.lisp --- SKEL Packages

;;

;;; Code:
(defpkg :skel/core
  (:use :std-lisp :ast :doc :log :config :project :schema :rdb :db :store :stored :id :vc :srv)
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
   :*default-skelfile* :*default-skel-user* 
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
   ;; objects
   :edit-skelrc :skel
   :def-sk-class :skel-project
   :scripts :skel-config :skel-store :user
   :skel-user-config :skel-system-config
   :*skel-user-config* :*skel-system-config*
   :print-skel-object
   ;; schema
   :*skel-registry-schema* :*skel-cache-schema*
   ;; db
   :skel-db :skel-db-path
   ;; log
   :*skel-log-schema*
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
   :setf-skel-vars
   :project-root
   ;; srv
   #:skel-service))

(defpkg :skel/comp/make
  (:use :std-lisp :skel/core :project :obj)
  (:import-from :doc :make-source-header-comment)
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
  (:export :rust-system))

(defpkg :skel/comp/python
  (:use :std-lisp :skel/core :toml :config :cli/tools/build :project)
  (:export :python-system))

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
   :project-org-file))

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

(defpkg :skel/net
  (:nicknames :sk-net)
  (:use :cl :std :net/srv/udp :skel/core :srv :log))

(defpkg :skel
  (:nicknames :sk)
  (:use :cl :std)
  (:use-reexport 
   :skel/core :skel/comp 
   :skel/net :project))

(defpkg :skel-user
  (:use :std-lisp :cli :tools
    :cl-user :log :sb-debug :sb-ext
    :obj :vc :rdb :io :net :pod :skel :doc))

(init :commands :name :skel :copy :cli :clean t)
