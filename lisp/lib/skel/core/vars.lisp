(in-package :skel/core/vars)

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

(defparameter *skel-stash* #P"/usr/local/share/skel/stash/")

(defparameter *skel-store* #P"/usr/local/share/skel/store/")

(defparameter *skel-cache* #P"/usr/local/share/skel/cache/")

(defparameter *skel-registry* #P"/usr/local/share/skel/registry/")

(defvar *skel-project*)
(defvar *skel-env*)

(defparameter *user-skelrc* (pathname (format nil "~~/~A" *default-skelrc*)))

(defparameter *system-skelrc* (pathname "/etc/skelrc"))

(defparameter *keep-ast* nil
  "Whether to keep the :ast slot stored with an sk object, or set it to nil so
that it can be GC'd.")

