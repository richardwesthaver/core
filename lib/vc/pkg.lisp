;;; vc/pkg.lisp --- Version Control Packages

;;; Code:
(defpackage :vc/proto
  (:use :cl :std :log :obj :parse/lex :dat/sxp)
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
   :*repo-auto-register*))

(defpackage :vc/hg
  (:use :cl :std :cli :sb-bsd-sockets :vc/proto :config)
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
   :hg-fast-export))

(defpackage :vc/git
  (:use :cl :std :cli :vc/proto :config)
  (:export :*git-program* 
   :git-repo :git-error 
   :run-git-command :git-meta :gitignore))

(defpackage :vc/util
  (:use :cl :std :cli :vc/proto :vc/git :vc/hg :config)
  (:export :make-hg-repo :make-git-repo :make-repo
           :find-repo-root
           :with-current-vc-root
           :with-repo))
   
(defpackage :vc/cli
  (:use :cl :std :cli :vc/proto :vc/git :vc/hg :vc/util)
  (:export :*vc-cli*))

(pkg:defpkg :vc
  (:use :cl :std)
  (:use-reexport :vc/proto :vc/hg :vc/git #+cli :vc/cli :vc/util))
