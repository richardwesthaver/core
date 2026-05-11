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

;;; Generics
(defgeneric vc-init (self)
  (:documentation "Initialize a vc-repo - calls either 'git init' or 'hg init'"))

(defgeneric vc-run (self cmd &rest args)
  (:documentation "Run a vc CMD with ARGS."))

(defgeneric vc-clone (self remote &key &allow-other-keys)
  (:documentation "Clone repo REMOTE into spec SELF."))

(defgeneric vc-push (self &key remote)
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

(defgeneric vc-purge (self &key &allow-other-keys)
  (:documentation "Purge the repo of unrecognized files. By default we only clear files and
directories which are ignored."))

(defgeneric vc-branch (self)
  (:documentation "Return the name of the current branch."))

(defgeneric vc-status (self &key &allow-other-keys))

(defgeneric vc-bundle (self output &key &allow-other-keys))
(defgeneric vc-unbundle (self input &key &allow-other-keys))

(defgeneric vc-export (self &key &allow-other-keys))

(defgeneric vc (self))
(defgeneric (setf vc) (new self))

;;; Accessors
(defgeneric vc-head (self))
(defgeneric vc-tags (self))
(defgeneric vc-revs (self))
(defgeneric vc-branches (self))
(defgeneric vc-remotes (self))
(defgeneric vc-remote (self cmd &key &allow-other-keys))
(defgeneric vc-submodule (self cmd &key &allow-other-keys))
(defgeneric vc-config (self))
(defgeneric (setf vc-config) (new self))
(defgeneric vc-type (self))
;;  IDEA 2023-12-29: :ediff t
(defgeneric vc-diff (a b &key &allow-other-keys))

;;; Objects

;;;; Config

;; usually parsed from .gitconfig or .hgrc
(defconfig vc-config () ())

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

(deftype vc-designator () `(or (member :hg :git) null)) ;; maybe: :sp (sapling)

(defstruct vc-remote 
  (type nil :type vc-designator) 
  name
  url)

(defaccessor name ((self vc-remote)) (vc-remote-name self))
(defaccessor uri ((self vc-remote)) (vc-remote-url self))

(defmethod print-object ((self vc-remote) stream)
  (let ((name (vc-remote-name self))
        (url (vc-remote-url self)))
    (format stream "(~A . ~A)" (string-downcase name) url)))

(defstruct vc-rev num id)

(defclass vc-repo ()
  ((path :initform nil :type (or null string pathname) :accessor path
         :initarg :path
         :documentation "AKA working-directory or working-copy")
   (head :initform nil :initarg :head :type (or null vc-rev) :accessor vc-head)
   (branches :initform (make-array 0 :element-type 'vc-branch :fill-pointer 0)
             :type (vector vc-branch) :accessor vc-branches :initarg :branches)
   (submodules :type (vector vc-repo) :accessor vc-submodules)
   (tags :initform (make-array 0 :element-type 'vc-tag :fill-pointer 0) :type (vector vc-tag) :accessor vc-tags)
   (revisions :initform (make-array 0 :element-type 'vc-rev :fill-pointer 0)
              :type (vector vc-rev) :accessor vc-revs)
   (remotes :initform (make-array 0 :element-type 'vc-remote :fill-pointer 0)
            :type (vector vc-remote) :accessor vc-remotes :initarg :remotes)
   (config :initform nil :type (or null vc-config) :accessor vc-config))
  (:documentation "generic Repository object backed by one of VC-DESIGNATOR."))

(defun register-repo (repo)
  "Register a repo, collecting information from the filesystem and
creating a repo object which is stored in *REPO-REGISTRY*."
  (setf (gethash (path repo) *repo-registry*) repo))

(defun find-repo (name)
  "Find a repo in *REPO-REGISTRY*."
  (gethash name *repo-registry*))

(defmethod initialize-instance :after ((self vc-repo) &key)
  (when *repo-auto-register* (register-repo self)))

(defmethod name ((self vc-repo))
  (car (last (pathname-directory (path self)))))

(defmethod vc-type ((self vc-repo)) t)

(defmethod write-ast ((self vc-repo) stream &key (pretty t) (case :downcase))
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

;; (defmethod print-object ((self vc-repo) stream)
;;   (print-unreadable-object (self stream)
;;     (write "vc-repo" :stream stream)
;;     (std:when-let ((remotes (vc-remotes self)))
;;       (write " " :stream stream)
;;       (pprint-tabular stream remotes nil nil 2))))

(defun find-repo-root (&optional (path *default-pathname-defaults*))
  "Check PATH for evidence of a VCS and continue walking up the filesystem until
we find one, else return NIL."
  (labels ((%check (dir)
             (if (null dir)
                 (return-from find-repo-root)
                 (if (probe-file (merge-pathnames ".hg/" dir))
                     :hg
                     (when (probe-file (merge-pathnames ".git/" dir))
                       :git)))))
    (let ((%path (directory-path (or path *default-pathname-defaults*))))
      (loop for x = (%check %path)
            for parent = (when-let ((parent (butlast (pathname-directory %path))))
                           (make-pathname :directory parent))
            if x
            return (values %path x)
            else if (or (not parent) (sequence:emptyp (namestring parent)))
            return nil
            else
            do (setf %path parent)))))

;;; Early Macro definition
(defmacro with-repo ((sym &rest args &key (path *default-pathname-defaults*) init type delete &allow-other-keys) &body body)
  `(unwind-protect
        (with-directory (probe-directory ,path)
          (let ((,sym ,@(or (when args 
                              (unless (keywordp (car args))
                                `(pop ,args)))
                            `((make-repo 
                               *default-pathname-defaults* 
                               ,@(when init `(:init ,init)) ,@(when type `(:type ,type)))))))
            (setf *repo* ,sym)
            ,@body))
     ,@(when delete `((probe-delete-directory ,path)))))
