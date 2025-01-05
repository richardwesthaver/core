;;; std/path.lisp --- Standard Path Library

;; Pathname support

;;; Commentary:


;;; Code:
(in-package :std/path)

(defgeneric path (self)
  (:method ((self string))
    (pathname self)))

(defun symlinkp (pathname)
  (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat pathname))))

(deftype wild-pathname ()
  "A pathname with wild components."
  '(and pathname (satisfies wild-pathname-p)))

(deftype non-wild-pathname ()
  "A pathname without wild components."
  '(or directory-pathname
    (and pathname (not (satisfies wild-pathname-p)))))

(deftype absolute-pathname ()
  '(and pathname (satisfies uiop:absolute-pathname-p)))

(deftype relative-pathname ()
  '(and pathname (satisfies uiop:relative-pathname-p)))

(deftype directory-pathname ()
  '(and pathname (satisfies uiop:directory-pathname-p)))

(deftype symlink-pathname ()
  '(and pathname (satisfies symlinkp)))

(deftype absolute-directory-pathname ()
  '(and absolute-pathname directory-pathname))

(deftype file-pathname ()
  '(and pathname (satisfies uiop:file-pathname-p)))

;; logical-pathname is defined in CL.

(defconstant +pathsep+
  #+windows #\; #+unix #\:
  "Path separator for this OS.")

(defconstant +wildfile+ (make-pathname :name :wild :type :wild :version :wild))

(defun merge-homedir-pathnames (pathname &optional (default-version :newest))
  (merge-pathnames pathname (user-homedir-pathname) default-version))

;; from UIOP
(defun set-pathname-suffix (path suffix &rest keys)
  (apply 'make-pathname :name (concatenate 'string (pathname-name path) suffix)
                        :defaults path keys))

(defvar *tmp-suffix* "-tmp")
(defvar *tmp* #P"/tmp/")

;; from UIOP
(defun tmpize-pathname (path)
  "Return a new pathname based on PATH and *TMP-SUFFIX* with a gensym'd integer
appended."
  (set-pathname-suffix path (symbol-name
                             (gensym *tmp-suffix*))))

(defmacro with-directory (dir &body body)
  `(let ((*default-pathname-defaults* ,dir))
     ,@body))

(defmacro with-tmp (&body body)
  `(with-directory *tmp*
     ,@body))

;;; Walkers
;; From UIOP:COLLECT-SUB*DIRECTORIES
(defun walk-directory (directory collectp recursep collector)
  "Given a DIRECTORY, when COLLECTP returns true when APPLY'ed with the directory,
call-function the COLLECTOR function designator on the directory,
and recurse each of its subdirectories on which the RECURSEP returns true when APPLY'ed with them.

The behavior in presence of symlinks is not portable."
  (when (apply collectp directory)
    (apply collector directory)
    (dolist (subdir (subdirectories directory))
      (when (apply recursep subdir)
        (walk-directory subdir collectp recursep collector)))))
