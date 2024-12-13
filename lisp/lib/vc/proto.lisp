;;; lib/vc/proto.lisp --- VC Protocol

;;

;;; Code:
(in-package :vc/proto)

;;; Vars
(defvar *default-vc-kind* :hg)
(defvar *repo* nil)
(defvar *repo-roots* nil)
(defvar *repo-registry* (make-hash-table :test 'equal))
(defvar *repo-auto-register* t
  "When non-nil, register all VC-REPO objects when they are created.")

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

(defgeneric vc-clone (self remote &key &allow-other-keys)
  (:documentation "Clone repo REMOTE into spec SELF."))

(defgeneric vc-push (self &optional remote)
  (:documentation "Push repo SELF to REMOTE."))

(defgeneric vc-pull (self &optional remote)
  (:documentation "Pull repo REMOTE into spec SELF."))

(defgeneric vc-update (self &optional branch)
  (:documentation "Update repo SELF with optional BRANCH."))

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

(defgeneric vc-bundle (self output &key &allow-other-keys))
(defgeneric vc-unbundle (self input &key &allow-other-keys))

;;; Accessors
(defgeneric vc-head (self))
(defgeneric vc-tags (self))
(defgeneric vc-revs (self))
(defgeneric vc-branches (self))
(defgeneric vc-remotes (self))
(defgeneric vc-config (self))
(defgeneric vc-type (self))
;;  IDEA 2023-12-29: :ediff t
(defgeneric vc-diff (a b &key &allow-other-keys))

;;; Objects

;;;; Config

;; usually parsed from .gitconfig or .hgrc
(defclass vc-config (config) ())

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

(deftype vc-designator () `(member :hg :git list)) ;; maybe: :sp (sapling)

(defclass vc-repo ()
  ((path :initform nil :type (or null string pathname) :accessor path
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

(defun register-repo (repo)
  "Register a repo, collecting information from the filesystem and
creating a repo object which is stored in *REPO-REGISTRY*."
  (setf (gethash (path repo) *repo-registry*) repo))

(defun find-repo (name)
  "Find a repo in *REPO-REGISTRY*."
  (gethash name *repo-registry*))

(defmethod :after initialize-instance ((self vc-repo) &key)
  (when *repo-auto-register* (register-repo self)))

(defmethod name ((self vc-repo))
  (car (last (pathname-directory (path self)))))

(defmethod vc-type ((self vc-repo)) t)

(defmethod write-sxp-stream ((self vc-repo) stream &key (pretty t) (case :downcase))
  (if (= 0 (length (vc-remotes self)))
      (write (vc-type self) :stream stream :pretty pretty :case case :readably t :array t :escape t)
      (progn
        (format stream "(")
        (write (vc-type self) :stream stream :pretty pretty :case case :readably t :array t :escape t)
        (format stream " ")
        (loop for x in (vc-remotes self)
              do 
                 (write `(,(vc-type self) ,(coerce (vc-remotes self) 'list)) :stream stream :pretty pretty :case case :readably t :array t :escape t))
        (format stream ")"))))

(defmethod print-object ((self vc-repo) stream)
  (print-unreadable-object (self stream)
    (format stream "~S" (vc-type self))
    (when-let ((remotes (vc-remotes self)))
      (format stream " ~A" remotes))))
