(defpackage :vc
  (:use :cl :std :cli :log :obj :sb-bsd-sockets)
  (:export 
   :vc-error :git-error :hg-error :vc-status
   :vc-clone :vc-push :vc-pull :vc-commit :vc-branch
   :*hg-program* :*git-program*
   :run-hg-command :run-git-command
   :repo :hg-repo :git-repo
   :vc-meta :hg-meta :git-meta
   :hg-client :make-hg-client))
