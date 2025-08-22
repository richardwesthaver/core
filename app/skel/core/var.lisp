;;; var.lisp --- Skel Core Vars

;; 

;;; Code:
(in-package :skel/core/int)

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

(defvar *skel-init-hook* nil)

(defvar *skel-project* nil)
(defvar *skel-registry* nil)
(defvar *skel-env* (make-hash-table :test 'equal)
  "A hash-table containing active SKEL environment variables. Keys and values are
strings.

The environment can be used for example in SB-EXT:RUN-PROGRAM by running the
table through CLI/ENV:CONCAT-ENV-TABLE and passing it as the value of the
:ENVIRONMENT keyword argument.")

(defvar *skel-load-recursive* t
  "Whether to recursively load sk objects in the :include slot or store them
uninitialized with non-nil :ast slots.")

(defvar *default-skel-bindings* nil)

(defvar *skel-project-macros* nil)
(defvar *skel-project-symbol-macros* nil)
(defvar *skel-project-functions* nil)

(declaim (pathname *skel-stash* *skel-store*
               *skel-cache* *user-skelrc* *system-skelrc*))
(sb-ext:define-load-time-global *skel-stash* (merge-homedir-pathnames ".stash/skel/stash/"))
(sb-ext:define-load-time-global *skel-store* (merge-homedir-pathnames  ".stash/skel/store/"))
(sb-ext:define-load-time-global *skel-cache* (merge-homedir-pathnames ".stash/skel/cache/"))
(sb-ext:define-load-time-global *skel-data* (merge-homedir-pathnames ".stash/skel/data/"))
(sb-ext:define-load-time-global *skel-path* *default-pathname-defaults*)
(sb-ext:define-load-time-global *user-skelrc* (pathname (format nil "~~/~A" *default-skelrc*)))
(sb-ext:define-load-time-global *system-skelrc* (pathname "/etc/skelrc"))
