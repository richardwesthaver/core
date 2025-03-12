;;; skel.lisp --- Skel Top-level

;; Top-level commands for interacting with the SKEL system.

;;; Code:
(pkg:defpkg :skel
  (:nicknames :sk)
  (:use :cl :std)
  (:use-reexport 
   :skel/core :skel/comp 
   #+net :skel/net
   #+cli :skel/cli)
  (:export :with-project))

(pkg:defpkg :sk-user
  (:use :cl :std :std-user :cli :cl-user :log :sb-debug :sb-ext :net/proto/dns :cli/tools/sbcl :pod :cli/clap)
  (:use :skel :skel/core :skel/comp :skel/net))

(in-package :skel)

(pushnew :skel *features*)

#+cli
(progn
  #+rdb 
  (cli:defcmd skc-db ())
  #+clim
  (cli:defcmd skc-view ()
    (if cli:*args* 
        (let ((stuff (loop for a in cli:*args*
                           collect (skel::sk-project-slot a))))
          (skel/tools/view:sk-view (if (= 1 (length stuff)) (car stuff) stuff)))
        (skel/tools/view:sk-view (if (boundp '*skel-project*) *skel-project*
                     (if (boundp '*skel-user-config*) *skel-user-config*
                         (if (boundp '*skel-system-config*) *skel-system-config*
                             (skel-simple-error "skel config files not installed")))))))
  #+net
  (cli:defcmd skc-net ())
  (cli:defcmd skc-serve ())
  (cli:load-package-cli 
   *skel-cli*
   :cmds 
   (#+rdb (:name db :description "interact with the skel database" :thunk skc-db)
    #+clim (:name view :description "view an object in a new window" :thunk skc-view)
    #+net (:name net :description "communicate with skel clients and servers"
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
    (loop with elt = (car kw)
          while (keywordp elt)
          do 
             (progn
               (push (pop lst) kw)
               (push (pop lst) kw)))
    (values kw lst)))

(defmacro with-project (ctx &body body)
  `(let* ((*skel-project* ,(find-skelfile (or ctx *default-pathname-defaults*) :load t))
          (*default-pathname-defaults* (sk-src *skel-project*)))
     ,@body))
