;;; lib/skel/core/vc/proto.lisp --- VC Protocol

;;

;;; Code:
(in-package :skel/core)

(defgeneric vc-clone (repo &key &allow-other-keys))

(defgeneric vc-push (repo &key &allow-other-keys))

(defgeneric vc-pull (repo &key &allow-other-keys))

(defgeneric vc-commit (repo &key &allow-other-keys))

(defgeneric vc-branch (repo &key &allow-other-keys))

(defgeneric vc-status (repo &key &allow-other-keys))

(defgeneric vc-run (repo command &rest args)
  (:documentation "Run a vc COMMAND with REPO and ARGS."))

