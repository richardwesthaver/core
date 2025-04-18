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

(defconstant +pathsep+
  #+windows #\; #+unix #\:
  "Path separator for this OS.")

(defconstant +wildfile+ (make-pathname :name :wild :type :wild :version :wild))

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
  (merge-pathnames pathname (user-homedir-pathname) default-version))

(defun ensure-directory-truename (path &key verbose (mode 511))
  (truename (ensure-directories-exist (directory-path path) :verbose verbose :mode mode)))

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
  `(let ((*default-pathname-defaults* (probe-file ,dir)))
     ,@body))

(defmacro with-tmp (&body body)
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

(defun directory-empty-p (&optional (dir *default-pathname-defaults*))
  "Return non-nil if DIR is a DIRECTORY-PATHNAME which does not contain any files
or directories."
  (and (directory-path-p dir) (not (directory (merge-pathnames "*" dir)))))
