;;; obj/build.lisp --- Builder API

;;

;;; Code:
(in-package :obj/build)

(defgeneric build (self &key &allow-other-keys))
(defgeneric build-from (self from &key &allow-other-keys))
