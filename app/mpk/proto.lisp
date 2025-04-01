;;; proto.lisp --- MPK Protocols

;; 

;;; Code:
(in-package :mpk)

(defclass mpk-object (id) ())

(defclass mpk-project (mpk-object skel:sk-project) ())

(defgeneric mpk-play (self &rest args &key &allow-other-keys))
(defgeneric mpk-pause (self &rest args &key &allow-other-keys))
(defgeneric mpk-stop (self &rest args &key &allow-other-keys))
(defgeneric mpk-shuffle (self &rest args &key &allow-other-keys))
(defgeneric mpk-previous (self &rest args &key &allow-other-keys))

