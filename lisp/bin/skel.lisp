;;; Code:

;;  TODO 2024-05-09: add shell configurables to rules - maybe at sk-command
;;  level. :INPUT :WAIT :OUTPUT
(in-package :std-user)
(defpkg :bin/skel
  (:use :cl :std :cli :cli/clap/obj
   :vc :sb-ext :skel :log :cli/clap/util
   :obj/ast #+(and tools gui) :skel/tools/viz
   :db :rdb :schema :config :build :packy :krypt)
  (:import-from :cli/shell :*shell-input* :*shell-directory*)
  (:use :cli/tools/sbcl :cli/prompt))

(in-package :bin/skel)
(in-readtable :shell)

(defcmd skc-push ()
  (case (vc-type (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "push" *args* t))
    (:hg (run-hg-command "push" *args* t))
    (t (skel-simple-error "unknown VC type"))))

(defcmd skc-pull ()
  (case (vc-type (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "pull" *args* t))
    (:hg (run-hg-command "pull" (append '("-u") *args*) t))
    (t (skel-simple-error "unknown VC type"))))

(defcmd skc-clone ()
  (case (vc-type (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "clone" *args* t))
    (:hg (run-hg-command "clone" *args* t))
    (t (skel-simple-error "unknown VC type"))))

(defcmd skc-commit ()
  (case (vc-type (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "commit" (list "-m" (clap:getopt :message)) t))
    (:hg (run-hg-command "commit" (list "-m" (clap:getopt :message)) t))
    (t (skel-simple-error "unknown VC type"))))

(defcmd skc-vc ()
  (let* ((sk (find-skelfile #P"." :load t))
         (vc (vc-type (sk-vc sk))))
    (sb-ext:enable-debugger)
    (with-open-stream (proc (process-output 
                             (if-let ((cmd (pop *args*)))
                               (ecase vc
                                 (:hg (run-hg-command cmd *args* :stream))
                                 (:git (run-git-command cmd *args* :stream)))
                               (sb-ext:run-program (case vc (:hg *hg-program*) (:git *git-program*))
                                                   nil 
                                                   :output :stream))))
    (loop for x = (read-line proc nil)
          while x
          do (println x)))))

(defcmd skc-vc* ()
  (with-cli (*vc-cli* :args (cdr (cli:args)))
    (do-opts *cli*)
    (do-cmd *cli*)))

(defcmd skc-pk* ()
  (with-cli (*packy-cli* :args (cdr (cli:args)))
    (do-opts *cli*)
    (do-cmd *cli*)))

(defcmd skc-kr* ()
  (with-cli (*krypt-cli* :args (cdr (cli:args)))
    (do-opts *cli*)
    (blake3::load-blake3)
    (do-cmd *cli*)))

(defun sk-shell ()
  (trace! "starting skel shell")
  (setq *no-exit* t)
  (cli/clap::with-cli-handlers
    (progn
      (in-package :sk-user)
      (use-package :cl-user)
      (use-package :sb-ext)
      (use-package :std-user)
      (println "Welcome to SKEL")
      (sb-impl::toplevel-repl nil))))

(defcmd skc-shell () (sk-shell))

(load-package-cli 
 :skel
 :opts ((:name "interactive" 
         :description "enter the lisp image after running commands"))
 :cmds
 ((:name vc
   :description "version control"
   :thunk skc-vc*)
  (:name pk
   :description "packages"
   :thunk skc-pk*)
  (:name kr
   :description "cryptography"
   :thunk skc-kr*)
  (:name push
   :description "push the current project upstream"
   :thunk skc-push)
  (:name pull
   :description "pull the current project from remote"
   :thunk skc-pull)
  (:name clone
   :description "clone a remote project"
   :thunk skc-clone)
  (:name commit
   :description "commit changes to the project vc"
   :thunk skc-commit
   :opts ((:name "message" :description "commit message" :kind string)))
  (:name shell
   :description "open the sk-shell interpreter"
   :thunk skc-shell)
  #+(and tools gui)
  (:name view
   :description "View a skel object in the Skel Viewer GUI."
   :thunk skc-view)))

(defmain start-skel ()
  (in-package :sk-user)
  (in-readtable :shell)
  (with-cli ((package-cli :bin/skel) :args (cli:args))
    (do-opts *cli*)
    (init-skel)
    (setq *db* (make-db :skel))
    (do-cmd *cli*)
    (when (getopt "interactive" nil) 
      (sk-shell))))
