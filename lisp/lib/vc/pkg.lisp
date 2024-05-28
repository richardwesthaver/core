(defpackage :vc
  (:use :cl :std :cli :log :obj :sb-bsd-sockets :cl-ppcre :parse/lex)
  (:import-from :uiop :with-current-directory)
  (:export :*default-vc*
   :vc-error :git-error :hg-error :vc-status
   :vc-clone :vc-push :vc-pull :vc-commit
   :vc-init :vc-id :vc-add :vc-remove
   :vc-addremove :vc-diff
   :vc-branch :*hg-program* :*git-program* :run-git-command
   :run-hg-command :repo :hg-repo :git-repo
   :vc-meta :hg-meta :git-meta :make-hg-client
   :hg-client :*repo-roots* :*repo-registry* :find-repo
   :make-repo :register-repo
   :vc-ignore :hgignore :gitignore))

(in-package :vc)

(defparameter *default-vc* :hg)
