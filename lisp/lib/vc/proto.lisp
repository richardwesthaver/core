;;; lib/vc/proto.lisp --- VC Protocol

;;

;;; Code:
(in-package :vc/proto)

;;; Conditions
(define-condition vc-error (std-error) ())

;;; Utils
(defun namestring-or (obj)
  (if (pathnamep obj)
      (namestring obj)
      obj))

(defun rel-pathname (path)
  (pathname (string-left-trim '(#\/) path)))

(defun glob-path-match (glob)
  (lambda (p start end)
    (member (subseq p start end) (directory (rel-pathname glob)) :test 'equal)))

;;; Functions
(defgeneric vc-init (self)
  (:documentation "Initialize a vc-repo - calls either 'git init' or 'hg init'"))

(defgeneric vc-run (self cmd &rest args)
  (:documentation "Run a vc CMD with ARGS."))

(defgeneric vc-id (self)
  (:documentation "Get the ID of a vc object."))

(defgeneric (setf vc-id) (self id)
  (:documentation "Set the ID of a vc object."))

(defgeneric vc-clone (self remote &key &allow-other-keys)
  (:documentation "Clone repo REMOTE into spec SELF."))

(defgeneric vc-push (self &optional remote)
  (:documentation "Push repo SELF to REMOTE."))

(defgeneric vc-pull (self &optional remote)
  (:documentation "Pull repo REMOTE into spec SELF."))

(defgeneric vc-commit (self msg &key &allow-other-keys)
  (:documentation "Commit repo object SELF, supplied with message MSG."))

(defgeneric vc-add (self &rest files)
  (:documentation "Add FILES to repo SELF."))

(defgeneric vc-remove (self &rest files)
  (:documentation "Remove FILES from repo SELF."))

(defgeneric vc-addremove (self &rest files)
  (:documentation "Add any untracked files in the current directory and delete tracked files that
are missing."))

(defgeneric vc-branch (self)
  (:documentation "Return the name of the current branch."))

(defgeneric vc-status (self &key &allow-other-keys))

(defgeneric vc-bundle (self &key &allow-other-keys))
(defgeneric vc-unbundle (self &key &allow-other-keys))

;;; Accessors
(defgeneric vc-path (self))
(defgeneric vc-head (self))
(defgeneric vc-tags (self))
(defgeneric vc-revs (self))
(defgeneric vc-branches (self))
(defgeneric vc-remotes (self))
(defgeneric vc-config (self))

;;  IDEA 2023-12-29: :ediff t
(defgeneric vc-diff (a b &key &allow-other-keys))

;;; Objects

;;;; Config
;; should be parsed from .hgrc and .gitconfig
(defclass vc-config (sxp cfg) ())

;;;; Ignorefile

;; Basically we treat HG and GIT ignore files the same - just lines of string
;; patterns. HG uses regexp and GIT is globs - an IGNOREFILE has a line parser
;; slot for selecting the appropriate function.

(defun map-lines (fn path)
  "Call FN on each line of file PATH and collect the result."
  (with-open-file (file path)
    (loop for line = (read-line file nil)
          while line
          unless (or (= (length line) 0) (char= (aref line 0) #\#))
          collect (funcall fn line))))

(defstruct vc-ignore path patterns)

(defgeneric vc-path-ignored-p (obj path)
  (:documentation "Check PATH against the patterns in OBJ. If there is a match, return non-nil.")
  (:method ((obj vc-ignore) (path t))
    (let ((len (length path)))
      (loop for pat in (vc-ignore-patterns obj)
            when (funcall pat path 0 len)
            return (values path pat)))))

(defstruct vc-branch name rev)

(defstruct vc-commit id message)

(defstruct vc-tag name id)

(defstruct vc-remote name url)

(defstruct vc-rev num id)

(defclass vc-repo ()
  ((path :initform nil :type (or null string pathname) :accessor vc-path
         :initarg :path
         :documentation "AKA working-directory or working-copy")
   (head :initform nil :initarg :head :type (or null vc-rev) :accessor vc-head)
   (branches :initform (make-array 0 :element-type 'vc-branch :fill-pointer 0)
             :type (vector vc-branch) :accessor vc-branches)
   (tags :initform (make-array 0 :element-type 'vc-tag :fill-pointer 0) :type (vector vc-tag) :accessor vc-tags)
   (revisions :initform (make-array 0 :element-type 'vc-rev :fill-pointer 0)
              :type (vector vc-rev) :accessor vc-revs)
   (remotes :initform (make-array 0 :element-type 'vc-remote :fill-pointer 0)
            :type (vector vc-remote) :accessor vc-remotes)
   (config :initform nil :type (or null vc-config) :accessor vc-config))
  (:documentation "generic Repository object backed by one of VC-DESIGNATOR."))

(defmethod vc-init ((self (eql t)))
  (make-instance 'vc-repo))
