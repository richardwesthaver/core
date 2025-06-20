;;; homer/cli.lisp --- Homer CLI

;; 

;;; Code:
(in-package :homer/cli)

;;; CLI
(defopt homer-force (when *arg* (setq *homer-force* t)))

(defcmd homer-show ()
  (if *args*
      (dolist (a *args*)
        (println (home-config-slot (keywordicate (string-upcase a)))))
      (describe *home-config*)))

(defcmd homer-check ()
  (let ((src (slot-value *home-config* 'homer/core::src)))
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapcar #'homer-status
                (find-files
                 *default-pathname-defaults*
                 *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcmd homer-push ()
  (let ((src (slot-value *home-config* 'homer/core::src)))
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapc #'homer-maybe-push
              (find-files src *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcmd homer-pull ()
  (let ((src (slot-value *home-config* 'homer/core::src)))
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapc #'homer-maybe-pull
              (find-files src *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcmd homer-run ()
  (mapcar 
   (lambda (x)
     (run-object
      (find (string-upcase x) (jobs *home-config*)
            :test 'equal
            :key (lambda (x) (homer/core::homer-job-target x)))))
   *args*))

(defcmd homer-start-cmd ()
  (start (find (string-upcase (car *args*)) (homer/core::services *home-config*)
               :test 'equal
               :key (lambda (y) (string (id:id y))))))

(defcmd homer-stop-cmd ()
  (stop (find (string-upcase (car *args*)) (homer/core::services *home-config*)
                      :test 'equal
                      :key (lambda (y) (string (id:id y))))))

(defcmd homer-restart-cmd ()
  (reset
   (find (string-upcase (car *args*)) (homer/core::services *home-config*)
         :test 'equal
         :key (lambda (y) (string (id:id y))))))

(defcmd homer-status-cmd ()
  (let ((srv (find (string-upcase (car *args*))
                   (homer/core::services *home-config*)
                   :test 'equal
                   :key (lambda (y) (string (id:id y))))))
    (homer/core::status srv)))

(defcmd homer-install ()
  (let ((src (slot-value *home-config* 'homer/core::src)))
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapc #'homer-maybe-install
              (find-files src *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(define-cli *homer-cli*
  :help t
  :name "homer"
  :version "0.1.0"
  :description "user home manager"
  :thunk homer-check
  :opts ((:name "level" :description "set the log level" :thunk level-opt)
         (:name "version" :description "print version" :thunk version-opt)
         (:name "ast" :description "keep ASTs" :thunk keep-ast-opt)
         (:name "force" :description "use force" :thunk homer-force))
  :cmds ((:name show :thunk homer-show)
         (:name check :thunk homer-check)
         (:name push :thunk homer-push)
         (:name pull :thunk homer-pull)
         (:name install :thunk homer-install)
         (:name run :thunk homer-run)
         (:name start :thunk homer-start-cmd)
         (:name restart :thunk homer-restart-cmd)
         (:name stop :thunk homer-stop-cmd)
         (:name status :thunk homer-status-cmd)))
