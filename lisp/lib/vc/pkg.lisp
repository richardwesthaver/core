(defpackage :vc/proto
  (:use :cl :std :log :obj :cl-ppcre :parse/lex)
  (:import-from :uiop :with-current-directory)
  (:export 
   :vc-repo
   :vc-run
   :vc-path
   :vc-head
   :vc-bundle :vc-unbundle
   :vc-error :vc-status
   :vc-clone :vc-push :vc-pull :vc-commit
   :vc-init :vc-id :vc-add :vc-remove
   :vc-addremove :vc-diff
   :vc-branch :repo
   :vc-meta :find-repo
   :make-repo :register-repo
   :vc-ignore))

(pkg:defpkg :vc/hg
  (:use :cl :std :cli :sb-bsd-sockets :vc/proto)
  (:export :*hg-program* :hg-repo :hg-error :run-hg-command :hg-meta :make-hg-client :hg-client :hgignore))

(defpackage :vc/git
  (:use :cl :std :cli :vc/proto)
  (:export :*git-program* :git-repo :git-error :run-git-command :git-meta :gitignore))

(pkg:defpkg :vc
  (:use :cl :std)
  (:use-reexport :vc/proto :vc/hg :vc/git)
  (:export :*default-vc-kind* :*repo-roots* :*repo-registry*
   :find-repo :register-repo :make-repo :make-hg-repo
   :make-git-repo))
