;;; lib/pod/buildah.lisp --- Pod Builder

;;

;;; Code:
(in-package :pod)

(defvar *buildah-exe* (find-exe "buildah"))
