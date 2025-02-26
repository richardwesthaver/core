;;; cli.lisp --- VC CLI Implementation

;; 

;;; Code:
(in-package :vc/cli)

(defcmd vc-status-cmd ()
  (vc-status *repo*))

(defcmd vc-commit-cmd ()
  (vc-commit *repo* (car *args*)))

(defcmd vc-pull-cmd ()
  (vc-pull *repo* (car *args*)))

(defcmd vc-push-cmd ()
  (vc-push *repo* (car *args*)))

(defcmd vc-addremove-cmd ()
  (apply 'vc-addremove *repo* *args*))

(defcmd vc-clone-cmd ()
  (vc-clone (make-instance 'vc-repo) (car *args*)))

(defcmd vc-fast-export-cmd ()
  (hg-fast-export (make-repo *default-pathname-defaults*) (car *args*)))

(defcmd vc-bundle-cmd ()
  (vc-bundle (make-repo *default-pathname-defaults*) (car *args*)))

(defcmd vc-unbundle-cmd ()
  (vc-unbundle (make-repo *default-pathname-defaults*) (car *args*)))

;; (defcmd vc-diff-cmd ()
;;   (vc-diff

(define-cli *vc-cli*
  :name "vc"
  :package :vc
  :help t
  :version 0
  :description "Version Controller"
  :thunk vc-status-cmd
  :cmds ((:name "status" :description "Print the status of the current repo"
          :thunk vc-status-cmd)
         (:name "push" :description "Push the current repo to a remote"
          :thunk vc-push-cmd)
         (:name "diff" :description "Perform a diff"
          :thunk vc-diff-cmd)
         (:name "pull" :description "Pull the current repo from a remote"
          :thunk vc-pull-cmd)
         (:name "clone" :description "Clone a repo from a remote" :thunk vc-clone-cmd)
         (:name "commit" :description "Commit the working set to the revision tree"
          :thunk vc-commit-cmd)
         (:name "addremove" :description "Add/remove files" :thunk vc-addremove-cmd)
         (:name "fast-export" :description "Run the hg-fast-export script"
          :thunk vc-fast-export-cmd)
         (:name "bundle" :description "Bundle a repo" :thunk vc-bundle-cmd)
         (:name "unbundle" :description "Unbundle a repo-bundle file" :thunk vc-unbundle-cmd)))
