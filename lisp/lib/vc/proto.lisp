;;; lib/vc/proto.lisp --- VC Protocol

;;

;;; Code:
(in-package :vc)

;;; Functions 
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

;;; Objects

;; should be parsed from .hgrc and .gitconfig
(defclass vc-config (sxp) ())

(defstruct vc-branch name rev)

(defstruct vc-commit id message)

(defstruct vc-tag name id)

(defstruct vc-remote name url)

(defstruct vc-rev num id)

(defclass vc-repo ()
  ((path :initform nil :type (or null string) :accessor vc-repo-path
         :initarg :path
         :documentation "AKA working-directory or working-copy")
   (head :initform nil :initarg :head :type (or null vc-rev) :accessor vc-repo-head)
   (branches :initform (make-array 0 :element-type 'vc-branch :fill-pointer 0) :type (vector vc-branch))
   (tags :initform (make-array 0 :element-type 'vc-tag :fill-pointer 0) :type (vector vc-tag))
   (revisions :initform (make-array 0 :element-type 'vc-rev :fill-pointer 0) :type (vector vc-rev))
   (remotes :initform (make-array 0 :element-type 'vc-remote :fill-pointer 0) :type (vector vc-remote))
   (config :initform nil :type (or null vc-config)))
  (:documentation "generic Repository object backed by one of VC-DESIGNATOR."))
