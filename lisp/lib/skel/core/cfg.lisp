(in-package :skel)
;;; Vars
(declaim (type vc-designator *default-skel-vc-kind*))
(deftype vc-designator () '(member :hg :git nil))
(defparameter *default-skel-vc-kind* :hg)

(declaim (type sk-project *skel-project*))
(defvar *skel-project*)
(declaim (type sk-user-config *skel-user-config*))
(defvar *skel-user-config*)

;; TODO (defparameter *skel-project-registry* nil)
;; TODO (defvar *skelfile-boundary* nil "Set an upper bounds on how
;; many times and how far to walk an arbitrary file directory.")

(declaim (type string *default-skel-user* *default-skelfile* *default-skel-extension*))
(defparameter *default-skel-user* (uid-username (getuid)))
(defparameter *default-skelfile* "skelfile")
(defparameter *default-skel-extension* "sk")
(defparameter *default-skelrc* ".skelrc")

(declaim (type pathname *default-skel-stash* *default-skel-shed*
	       *default-skel-cache* *default-user-skelrc* *default-system-skelrc*))
	       

(defparameter *default-skel-stash* (pathname (format nil "/home/~a/stash/" *default-skel-user*)))

(defparameter *default-skel-shed* (pathname (format nil "/home/~a/shed/" *default-skel-user*)))

(defparameter *default-skel-cache* (pathname (format nil "/home/~a/.cache/skel/" *default-skel-user*)))

(defparameter *default-user-skelrc* (pathname (format nil "/home/~A/~A" *default-skel-user* *default-skelrc*)))

(defparameter *default-system-skelrc* (pathname "/etc/skelrc"))
