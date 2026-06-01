;;; var.lisp --- Skel Core Vars

;; 

;;; Code:
(in-package :skel/core)

(defparameter *default-skel-license-kind* :mpl2)
;; TODO (defvar *skelfile-boundary* nil "Set an upper bounds on how
;; many times and how far to walk an arbitrary file directory.")
(declaim (type string *default-skel-user* *default-skelfile* *default-skel-extension*))
(defparameter *default-skel-user* (uid-username (unix-getuid)))
(defparameter *default-skelfile* "skelfile")
(defparameter *default-skel-extension* "sk")
(defparameter *default-skelrc* ".skelrc")

(defparameter *skel-hook* (make-instance 'std:key-hook))

(defvar *skel-project* nil)
(defvar *skel-registry* nil)
;; TODO 2026-06-01: replace with single var *skel-config*
(defvar *skel-user-config* nil)
(defvar *skel-system-config* nil)
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

(eval-always
  (defvar *skel-project-macros* nil)
  (defvar *skel-project-symbol-macros* nil)
  (defvar *skel-project-functions* nil))

(defun skel-config-directory () (merge-pathnames "skel/" (std:xdg-dir :config-home)))
  
(declaim (pathname *skel-stash* *skel-store*
                   *skel-cache* *system-skelrc* 
                   *user-skelrc*))
(defvar *skel-stash* (xdg-data-directory "skel/stash"))
(defvar *skel-store* (xdg-data-directory  "skel/store"))
(defvar *skel-cache* (xdg-cache-directory "skel"))
(defvar *skel-data* (xdg-data-directory "skel"))
(defvar *skel-path* *default-pathname-defaults*)
(defvar *system-skelrc* (pathname "/etc/skelrc"))
(defvar *user-skelrc* (xdg-config-file :skel))

(std:defvar-unbound *default-clean-function*
  "The default function used to clean a SK-PROJECT.")
