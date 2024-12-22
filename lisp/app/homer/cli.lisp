;;; cli.lisp --- Homer CLI

;; 

;;; Code:
(in-package :homer/cli)

;;; CLI
(defopt homer-version (print-version *cli*))
(defopt homer-ast (setq *keep-ast* (or *arg*)))
(defopt homer-log-level (when *arg* (setq *log-level* :debug)))
(defopt homer-force (when *arg* (setq *homer-force* t)))

(defcmd homer-show ()
  (describe *home-config*))

(defcmd homer-check ()
  (with-slots (src) *home-config*
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapcar #'homer-status
                (find-files
                 *default-pathname-defaults*
                 *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcmd homer-push ()
  (with-slots (src) *home-config*
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapc #'homer-maybe-push
              (find-files src *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcmd homer-pull ()
  (with-slots (src) *home-config*
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapc #'homer-maybe-pull
              (find-files src *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcmd homer-install ()
  (with-slots (src) *home-config*
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
  :opts ((:name "level" :description "set the log level" :thunk homer-log-level)
         (:name "version" :description "print version" :thunk homer-version)
         (:name "force" :description "use force" :thunk homer-force))
  :cmds ((:name show :thunk homer-show)
         (:name check :thunk homer-check)
         (:name push :thunk homer-push)
         (:name pull :thunk homer-pull)
         (:name install :thunk homer-install)))
