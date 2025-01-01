;;; skel.lisp --- Skel Top-level

;; Top-level commands for interacting with the SKEL system.

;;; Code:
(pkg:defpkg :skel
  (:nicknames :sk)
  (:use :cl :std)
  (:use-reexport 
   :skel/core :skel/comp 
   #+rdb :skel/db 
   #+net :skel/net
   #+cli :skel/cli))

(pkg:defpkg :sk-user
  (:use :cl :std :std-user :cli :cl-user :log :sb-debug :sb-ext :net/proto/dns :cli/tools/sbcl :pod :cli/clap)
  (:use :skel :skel/core :skel/comp :skel/net))

(in-package :skel)

(pushnew :skel *features*)

#+cli
(progn
  #+rdb 
  (cli:defcmd skc-db ())
  #+gui
  (cli:defcmd skc-view ()
    (if cli:*args* 
        (let ((stuff (loop for a in *args*
                           collect (skel/cli::sk-slot-case a))))
          (skel/tools/viz:sk-view (if (= 1 (length stuff)) (car stuff) stuff)))
        (skel/tools/viz:sk-view (if (boundp '*skel-project*) *skel-project*
                     (if (boundp '*skel-user-config*) *skel-user-config*
                         (if (boundp '*skel-system-config*) *skel-system-config*
                             (skel-simple-error "skel config files not installed")))))))

  #+net
  (cli:defcmd skc-net ())
  (cli:load-package-cli 
   *skel-cli*
   :cmds 
   (#+rdb (:name db :description "interact with the skel database" :thunk skc-db)
    #+gui (:name view :description "view an object in a new window" :thunk skc-view)
    #+net (:name net :description "communicate with skel clients and servers" 
           :thunk skc-net))))

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
  `(let ((*skel-project* (or *skel-project* (find-skelfile ,(or ctx *default-pathname-defaults*) :load t))))
     ,@body))
