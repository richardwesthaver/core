;;; vc/pkg.lisp --- Version Control Packages

;;; Code:
(defpackage :vc/proto
  (:use :cl :std :log :obj :parse/lex)
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
   :make-repo :register-repo
   :vc-update
   :vc-ignore
   :vc-remotes
   :vc-designator
   :vc-type
   :*repo*
   :*default-vc-kind*
   :*repo-roots*
   :*repo-registry*
   :*repo-auto-register*
   :find-repo :register-repo))

(pkg:defpkg :vc/hg
  (:use :cl :std :cli :sb-bsd-sockets :vc/proto)
  (:export :*hg-program* :hg-repo :hg-error :run-hg-command :hg-meta :make-hg-client :hg-client :hgignore))

(defpackage :vc/git
  (:use :cl :std :cli :vc/proto)
  (:export :*git-program* :git-repo :git-error :run-git-command :git-meta :gitignore))
   
(defpackage :vc/cli
  (:use :cl :std :cli :vc/proto :vc/git :vc/hg)
  (:export :*vc-cli*))
