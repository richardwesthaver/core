;;; lib/skel/core/vc/obj.lisp --- VC Objects

;;

;;; Code:
(in-package :skel/core)

(defclass repo (skel sk-meta)
  ((head)
   (ignore)
   (branches)
   (tags)
   (revisions)
   (subrepos)
   (remotes)
   (config))
  (:documentation "generic Repository object backed by one of VC-DESIGNATOR."))
