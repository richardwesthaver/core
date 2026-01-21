;;; cli.lisp --- VC CLI Implementation

;; 

;;; Code:
(in-package :vc/cli)

(defcommand (:vc status) ()
  (vc-status *repo*))

(defcommand (:vc commit) (msg)
  (vc-commit *repo* msg))

(defcommand (:vc pull) (remote)
  (vc-pull *repo* remote))

(defcommand (:vc push) (&optional remote)
  (if remote
      (apply 'vc-push *repo* :remote remote)
      (vc-push *repo*)))

(defcommand (:vc addremove) (&rest args)
  (apply 'vc-addremove *repo* args))

(defcommand (:vc clone) (remote)
  (vc-clone (make-instance 'vc-repo) remote))

(defcommand (:vc fast-export) (&optional output)
  (hg-fast-export (make-repo *default-pathname-defaults*) output))

(defcommand (:vc bundle) (output)
  (vc-bundle (make-repo *default-pathname-defaults*) output))

(defcommand (:vc unbundle) (input)
  (vc-unbundle (make-repo *default-pathname-defaults*) input))

#+todo
(define-cli "vc")
