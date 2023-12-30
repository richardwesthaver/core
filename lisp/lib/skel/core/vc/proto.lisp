;;; lib/skel/core/vc/proto.lisp --- VC Protocol

;;

;;; Code:
(in-package :skel/core)

(defgeneric vc-init (self)
  (:documentation "Initialize a vc-repo - calls either 'git init' or 'hg init'"))

(defgeneric vc-run (self cmd &rest args)
  (:documentation "Run a vc COMMAND with REPO and ARGS."))

(defgeneric vc-id (self)
  (:documentation "Get the ID of a vc object."))

(defgeneric (setf vc-id) (self id)
  (:documentation "Set the ID of a vc object."))

(defgeneric vc-clone (self remote &key &allow-other-keys))

(defgeneric vc-push (self remote &key &allow-other-keys))

(defgeneric vc-pull (self remote &key &allow-other-keys))

(defgeneric vc-commit (self msg &key &allow-other-keys))

(defgeneric vc-add (self &rest files))

(defgeneric vc-remove (self &rest files))

(defgeneric vc-addremove (self &rest files))

(defgeneric vc-branch (self &key cmd branch &allow-other-keys))

(defgeneric vc-status (self &key &allow-other-keys))

;;  IDEA 2023-12-29: :ediff t
(defgeneric vc-diff (a b &key &allow-other-keys))
