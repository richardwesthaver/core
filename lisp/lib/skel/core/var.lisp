(in-package :skel/core/var)

(declaim (type vc-designator *default-skel-vc-kind*))
(defparameter *default-skel-vc-kind* :hg)
(defparameter *default-skel-license-kind* :mpl2)
;; TODO (defvar *skelfile-boundary* nil "Set an upper bounds on how
;; many times and how far to walk an arbitrary file directory.")
(declaim (type string *default-skel-user* *default-skelfile* *default-skel-extension*))
(defparameter *default-skel-user* (uid-username (unix-getuid)))
(defparameter *default-skelfile* "skelfile")
(defparameter *default-skel-extension* "sk")
(defparameter *default-skelrc* ".skelrc")

(declaim (type pathname *skel-stash* *skel-store*
               *skel-cache* *user-skelrc* *system-skelrc*))

(defvar *skel-stash* (merge-homedir-pathnames ".stash/skel/stash/"))

(defvar *skel-store* (merge-homedir-pathnames  ".stash/skel/store/"))

(defvar *skel-cache* (merge-homedir-pathnames ".stash/skel/cache/"))

(defvar *skel-registry* (merge-homedir-pathnames ".stash/skel/registry/"))

(defvar *skel-path* *default-pathname-defaults*)

(defvar *skel-project* nil)

(defvar *skel-env* (make-hash-table :test 'equal)
  "A hash-table containing active SKEL environment variables. Keys and values are
strings.

The environment can be used for example in SB-EXT:RUN-PROGRAM by running the
table through CLI/ENV:CONCAT-ENV-TABLE and passing it as the value of the
:ENVIRONMENT keyword argument.")

(defvar *user-skelrc* (pathname (format nil "~~/~A" *default-skelrc*)))

(defvar *system-skelrc* (pathname "/etc/skelrc"))

(defvar *skel-load-recursive* t
  "Whether to recursively load sk objects in the :include slot or store them
uninitialized with non-nil :ast slots.")
