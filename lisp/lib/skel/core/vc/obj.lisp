;;; lib/skel/core/vc/obj.lisp --- VC Objects

;;

;;; Code:
(in-package :skel/core)

;; should be parsed from .hgrc and .gitconfig
(defclass vc-config (skel sxp) ())

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
