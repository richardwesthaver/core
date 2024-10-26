;;; skel.lisp --- Skel Top-level

;; Top-level commands for interacting with the SKEL system.

;;; Code:
(pkg:defpkg :skel
  (:nicknames :sk)
  (:use :cl :std)
  (:use-reexport :skel/core :skel/comp))

(pkg:defpkg :sk-user
  (:use :cl :std :std-user :cli :cl-user :log :sb-debug :sb-ext :net/proto/dns :net/fetch :cli/tools/sbcl :pod :cli/clap)
  (:use :skel :skel/core :skel/comp :skel/net))

(in-package :skel)

(defvar *skel-init-keywords* '(:config *skel-user-config* 
                               :project *skel-project*
                               :cache *skel-cache*
                               :store *skel-store*
                               :stash *skel-stash*
                               :registry *skel-registry*
                               :cd *default-pathname-defaults*))

(eval-always
  (defun skel-keywordp (kw)
    (getf *skel-init-keywords* kw))
  (defun apply-skel-keywords (lst)
    ;; kludge
    (setf-skel-vars)
    (let ((kw))
      (loop with elt = (car kw)
            while (keywordp elt)
            do 
               (progn
                 (push (pop lst) kw)
                 (push (pop lst) kw)))
      (values kw lst))))

(defmacro with-skel (ctx &body body)
  `(let ((*skel-project* (find-skelfile ,(or ctx *default-pathname-defaults* :load t))))
     ,@body))
