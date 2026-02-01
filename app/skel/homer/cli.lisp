;;; homer/cli.lisp --- Homer CLI

;; 

;;; Code:
(in-package :skel/homer/cli)

;;; CLI
(init :commands :name :homer :copy :skel :reset t)

(define-command-type (:homer force) (&optional val) (when val (setq *homer-force* t)))

(defcommand (:homer show) (&rest args)
  (if args
      (dolist (a args)
        (println (home-config-slot (keywordicate (string-upcase a)))))
      (describe *home-config*)))

(defcommand (:homer check) ()
  (let ((src (slot-value *home-config* 'skel/homer/core::src)))
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapcar #'homer-status
                (find-files
                 *default-pathname-defaults*
                 *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcommand (:homer push) ()
  (let ((src (slot-value *home-config* 'skel/homer/core::src)))
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapc #'homer-maybe-push
              (find-files src *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcommand (:homer pull) ()
  (let ((src (slot-value *home-config* 'skel/homer/core::src)))
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapc #'homer-maybe-pull
              (find-files src *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defcommand (:homer run) (&rest args)
  (mapcar 
   (lambda (x)
     (run-object
      (find (string-upcase x) (jobs *home-config*)
            :test 'equal
            :key (lambda (x) (skel/homer/core::homer-job-target x)))))
   args))

(defcommand (:homer start) (srv)
  (start (find (string-upcase srv) (skel/homer/core::services *home-config*)
               :test 'equal
               :key (lambda (y) (string (id:id y))))))

(defcommand (:homer stop) (srv)
  (stop (find (string-upcase srv) (skel/homer/core::services *home-config*)
                      :test 'equal
                      :key (lambda (y) (string (id:id y))))))

(defcommand (:homer restart) (srv)
  (reset
   (find (string-upcase srv) (skel/homer/core::services *home-config*)
         :test 'equal
         :key (lambda (y) (string (id:id y))))))

(defcommand (:homer status) (srv)
  (let ((srv (find (string-upcase srv)
                   (skel/homer/core::services *home-config*)
                   :test 'equal
                   :key (lambda (y) (string (id:id y))))))
    (skel/homer/core::homer-status srv)))

(defcommand (:homer install) ()
  (let ((src (slot-value *home-config* 'skel/homer/core::src)))
    (if-let ((src (probe-file src)))
      (let ((*default-pathname-defaults* src))
        (mapc #'homer-maybe-install
              (find-files src *home-hidden-paths*)))
      (error 'file-error :pathname src))))

(defmain start-homer (:readtable :shell :package :homer :commands :homer :cli :homer)
  (let ((*print-readably* t))
    (init* :xdg :homer)
    (load-homerc)
    (call-interactively (or (second *posix-argv*) "show") (cddr *posix-argv*))))

(define-cli "homer" #'start-homer
  :version "0.1.0"
  :description "user home manager")

(save :commands :homer)
