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
