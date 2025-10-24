;;; std/path.lisp --- Standard Path Library

;; Pathname support

;;; Commentary:


;;; Code:
(in-package :std/path)

(defgeneric path (self)
  (:method ((self string))
    (pathname self))
  (:documentation "Return the path associated with SELF."))

(defun symlinkp (path)
  "Return T if PATH is a symlink."
  (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat path))))

(deftype wild-pathname ()
  "A pathname with wild components."
  '(and pathname (satisfies wild-pathname-p)))

(deftype non-wild-pathname ()
  "A pathname without wild components."
  '(or directory-pathname
    (and pathname (not (satisfies wild-pathname-p)))))

(deftype absolute-pathname ()
  "An absolute pathname."
  '(and pathname (satisfies uiop:absolute-pathname-p)))

(deftype relative-pathname ()
  "A relative pathname."
  '(and pathname (satisfies uiop:relative-pathname-p)))

(deftype directory-pathname ()
  "A directory pathname."
  '(and pathname (satisfies uiop:directory-pathname-p)))

(deftype symlink-pathname ()
  "A symlink pathname."
  '(and pathname (satisfies symlinkp)))

(deftype absolute-directory-pathname ()
  "An absolute directory pathname."
  '(and absolute-pathname directory-pathname))

(deftype file-pathname ()
  "A file pathname."
  '(and pathname (satisfies uiop:file-pathname-p)))

(defconstant +pathsep+
  #+windows #\; #+unix #\:
  "Path separator for this OS.")

(defconstant +wildfile+ (make-pathname :name :wild :type :wild :version :wild)
  "Constant wild file pathname specifier.")

(defun directory-path-p (path)
  "Return T if PATH is a directory else NIL."
  (declare (type (or pathname string) path))
  (and (not (pathname-name path))
       (not (pathname-type path))))

(defun directory-path (path)
  "If PATH is a directory pathname, return it as it is. If it is a file
pathname or a string, transform it into a directory pathname."
  (declare (type (or pathname string) path))
  (if (directory-path-p path)
      path
      (make-pathname :directory (append (or (pathname-directory path)
                                            (list :relative))
                                        (list (file-namestring path)))
                     :name nil :type nil :defaults path)))

(defun merge-homedir-pathnames (pathname &optional (default-version :newest))
  "Merge PATHNAME on USER-HOMEDIR-PATHNAME."
  (merge-pathnames pathname (user-homedir-pathname) default-version))

(defun ensure-directory-truename (path &key verbose (mode 511))
  "Ensure directory PATH exists and return its truename."
  (truename (ensure-directories-exist (directory-path path) :verbose verbose :mode mode)))

;; from UIOP
(defun set-pathname-suffix (path suffix &rest keys)
  "Return a pathname like PATH with a custom SUFFIX."
  (apply 'make-pathname :name (concatenate 'string (pathname-name path) suffix)
                        :defaults path keys))

(defvar *tmp-suffix* "-tmp" "Default suffix for TMPIZE-PATHNAME")
(defvar *tmp* #P"/tmp/" "Default temporary directory pathname.")

;; from UIOP
(defun tmpize-pathname (path)
  "Return a new pathname based on PATH and *TMP-SUFFIX* with a gensym'd integer
appended."
  (set-pathname-suffix path (symbol-name
                             (gensym *tmp-suffix*))))

(defun call-with-directory (dir thunk)
  "call the THUNK in a context where the current directory was changed to DIR, if not NIL.
Note that this operation is usually NOT thread-safe."
  (if dir
      (let* ((dir (directory-path (probe-file dir)))
             (cwd (sb-posix:getcwd))
             (*default-pathname-defaults* dir))
        (sb-posix:chdir dir)
        (unwind-protect
             (funcall thunk)
          (sb-posix:chdir cwd)))
      (funcall thunk)))

(defmacro with-directory (dir &body body)
  "Call BODY while the POSIX current working directory is set to DIR"
  `(call-with-directory ,dir #'(lambda () ,@body)))

(defmacro with-tmp (&body body)
  "Bind *DEFAULT-PATHNAME-DEFAULTS* to *TMP* around BODY."
  `(with-directory *tmp*
     ,@body))

;;; Walkers
;; From UIOP:COLLECT-SUB*DIRECTORIES
(defun walk-directory (directory collectp recursep collector)
  "Given a DIRECTORY, when COLLECTP returns true,
call the COLLECTOR function designator with the directory and recurse each of
its subdirectories on which RECURSEP returns true.

COLLECTP, RECURSEP, and COLLECT all take a single pathname (the directory) as
their only argument."
  (when (funcall collectp directory)
    (funcall collector directory)
    (dolist (subdir (subdirectories directory))
      (when (funcall recursep subdir)
        (walk-directory subdir collectp recursep collector)))))

;; TODO 2025-10-22: PWALK

;;; Directory Wildcards
(defun directory-empty-p (&optional (dir *default-pathname-defaults*))
  "Return non-nil if DIR is a DIRECTORY-PATHNAME which does not contain any files
or directories."
  (and (directory-path-p dir) (not (directory (merge-pathnames "*" dir)))))

;; from StumpWM 
;; ref: stumpwm/pathnames.lisp
(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name :wild
                 :type :wild
                 :defaults (directory-path dirname)))
