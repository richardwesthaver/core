;;; vc/pkg.lisp --- Version Control Packages

;;; Code:
(defpackage :vc/proto
  (:use :cl :std :log :parse/lex :dat/sxp :obj)
  (:export 
   :vc-repo
   :vc-run
   :vc-path
   :vc-head
   :vc-name
   :vc-bundle :vc-unbundle
   :vc-error :vc-status
   :vc-clone :vc-push :vc-pull :vc-commit
   :vc-init :vc-id :vc-add :vc-remove
   :vc-addremove :vc-diff
   :vc-branch :repo
   :vc-meta :find-repo
   :vc-export
   :make-repo :register-repo
   :vc-update
   :vc-ignore
   :vc-remotes
   :make-vc-remote
   :vc-designator
   :vc-type
   :*repo*
   :*default-vc-kind*
   :*repo-roots*
   :*repo-registry*
   :*repo-auto-register*
   :vc-remote
   :vc-submodule
   :vc-submodules
   :vc-config
   :find-repo-root))

(defpackage :vc/git
  (:use :cl :std :cli :vc/proto :config)
  (:export :*git-program* 
   :git-repo :git-error 
   :run-git-command :git-meta :gitignore :make-git-repo))

(defpackage :vc/hg
  (:use :cl :std :cli :sb-bsd-sockets :vc/proto :config :dat/toml :ast :uri)
  (:import-from :vc/git :run-git-command)
  (:export :*hg-program* 
   :hg-repo :hg-error 
   :run-hg-command :hg-meta 
   :make-hg-client :hg-client :hgignore
   :hg-bundle-type
   :hg-compression-engine
   :*hg-bundlespec-options*
   :hg-bundlespec-string-p
   :hg-config
   :*hg-fast-export-script*
   :hg-fast-export
   :find-hgrc
   :make-hg-repo
   :find-hg-bookmarks))

(defpackage :vc/util
  (:use :cl :std :cli :vc/proto :vc/git :vc/hg :config)
  (:import-from :uri :uri :uri-to-string)
  (:export :make-repo :with-current-vc-root :with-repo
   :directory-repos :bundle-repo
   :bundle-repos :update-repo
   :update-repos))
   
(defpackage :vc/cli
  (:use :cl :std :cli :clap :vc/proto :vc/git :vc/hg :vc/util)
  (:export :*vc-cli*))

(pkg:defpkg :vc
  (:use :cl :std)
  (:use-reexport :vc/proto :vc/hg :vc/git #+cli :vc/cli :vc/util))
