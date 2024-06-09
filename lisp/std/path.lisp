;;; std/path.lisp --- Standard Path Library

;;

;;; Code:
(in-package :std/path)

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

(deftype absolute-directory-pathname ()
  '(and absolute-pathname directory-pathname))

(deftype file-pathname ()
  '(and pathname (satisfies uiop:file-pathname-p)))

;; logical-pathname is defined in CL.

(defconstant +pathsep+
  #+windows #\; #+unix #\:
  "Path separator for this OS.")

(defconstant +wildfile+ (make-pathname :name :wild :type :wild :version :wild))

;; from UIOP:ADD-PATHNAME-SUFFIX
(defun set-pathname-suffix (path suffix &rest keys)
  (apply 'make-pathname :name (concatenate 'string (pathname-name path) suffix)
                        :defaults path keys))

(defvar *tmp-suffix* "-tmp")

;; based on UIOP:TMPIZE-PATHNAME
(defun tmpize-pathname (path)
  "Return a new pathname based on PATH and *TMP-SUFFIX* with a gensym'd integer
appended."
  (set-pathname-suffix path (symbol-name
                             (gensym *tmp-suffix*))))
