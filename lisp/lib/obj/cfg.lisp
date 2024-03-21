;;; obj/cfg.lisp --- Configuration flavors

;;

;;; Commentary:
;; The goal of this package is to make it easy to map an object in
;; memory to a 'user config interface' - which could be a
;; configuration file, a datagram, CLI flags, etc.
;;
;; This package only provides the CFG protocol, for other packages to
;; consume.

;;; Usage: 
#|
|#
;;; Code:
(defclass cfg ()
  ())

(defgeneric make-cfg (obj &rest args &key &allow-other-keys))
(defgeneric find-cfg (kind obj &rest args &key &allow-other-keys))
(defgeneric cfg-find (obj key &key &allow-other-keys))
(defgeneric cfg-get (obj key))
(defgeneric (setf cfg-get) (obj key val))
