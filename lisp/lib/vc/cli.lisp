;;; cli.lisp --- VC CLI Implementation

;; 

;;; Code:
(in-package :vc/cli)

(defcmd vc-status-cmd ()
  (with-current-vc-root (vc)
    (vc-status vc)))

(defcmd vc-pull-cmd ()
  (with-current-vc-root (vc)
    (vc-pull vc (car *args*))))

(defcmd vc-push-cmd ()
  (with-current-vc-root (vc)
    (vc-push vc (car *args*))))

(defcmd vc-clone-cmd ()
  (vc-clone (make-instance 'vc-repo) (car *args*)))

(defcmd vc-fast-export-cmd ()
  (hg-fast-export (make-repo *default-pathname-defaults*) (car *args*)))

(define-cli *vc-cli*
  :name "vc"
  :help t
  :version 0
  :description "Version Controller"
  :thunk vc-status-cmd
  :cmds ((:name "status" :description "Print the status of the current repo"
          :thunk vc-status-cmd)
         (:name "push" :description "Push the current repo to a remote"
          :thunk vc-push-cmd)
         (:name "pull" :description "Pull the current repo from a remote"
          :thunk vc-pull-cmd)
         (:name "clone" :description "Clone a repo from a remote")
         (:name "fast-export" :description "Run the hg-fast-export script"
          :thunk vc-fast-export-cmd)))
