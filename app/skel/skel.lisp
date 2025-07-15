;;; skel.lisp --- Skel Top-level

;; Top-level commands for interacting with the SKEL system.

;;; Code:
(pkg:defpkg :skel
  (:nicknames :sk)
  (:use :cl :std)
  (:use-reexport 
   :skel/core :skel/comp 
   :skel/net
   :skel/cli)
  (:export :with-project))

(pkg:defpkg :sk-user
  (:use :cl :std :cli :clap :tools
   :cl-user :log :sb-debug :sb-ext
   :net/proto/dns :obj/ast :vc :rdb 
   :io :net :pod)
  (:import-from :uri :uri)
  (:use :skel :skel/core :skel/comp :skel/net))

(in-package :skel)

(pushnew :skel *features*)

(progn
  (clap:defcmd skc-db ())
  (clap:defcmd skc-net ())
  (clap:defcmd skc-serve ())
  (clap:load-package-cli 
   *skel-cli*
   :cmds 
   ((:name db :description "interact with the skel database" :thunk skc-db)
    (:name net :description "communicate with skel clients and servers"
           :thunk skc-net))))

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
                               :registry *skel-registry*
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
