;;; skel.lisp --- Skel Top-level

;; Top-level commands for interacting with the SKEL system.

;;; Code:
(in-package :skel)

(pushnew :skel *features*)

;; db is locked while skel is running, prevents multiple instances
;; #+rdb
;; (pushnew 'init-skel-logger *skel-init-hook*)
;; #+rdb
;; (pushnew 'sk-log-shutdown sb-ext:*exit-hooks*)

(defvar *skel-init-keywords* '(:config *skel-user-config* 
                               :project *skel-project*
                               :cache *skel-cache*
                               :store *skel-store*
                               :stash *skel-stash*
                               :cd *default-pathname-defaults*))

(defun skel-keywordp (kw)
  (getf *skel-init-keywords* kw))

(defun apply-skel-keywords (lst)
  ;; kludge
  (setf-skel-vars)
  (let ((kw))
    (loop with elt = (car lst)
          while (keywordp elt)
          do 
             (dotimes (i 2)
               (push (pop lst) kw)))
    (values kw lst)))

(defmacro with-project (ctx &body body)
  `(let* ((*skel-project* ,(find-skelfile (or ctx *default-pathname-defaults*) :load t))
          (*default-pathname-defaults* (sk-src *skel-project*)))
     ,@body))
