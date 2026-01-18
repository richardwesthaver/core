;;; cli.lisp --- Skel CLI Definitions

;; CLI implementation of Skel

;;; Code:
(in-package :skel/cli)

;; *SKEL-CLI* exposes access to the SKEL system from the CLI.
(defcommand (:skel init) ()
  (let ((file (when *args* (pop *args*)))
        (name (when (> *argc* 1) (pop *args*))))
    ;; TODO: test, may need to be sequential for side-effect of pop
    (handler-bind
        ((sb-ext:file-exists
           #'(lambda (s)
               (std:println (format nil "file already exists: ~A" 
                                    (or file *default-skelfile*)))
               (let ((f2 (read-line)))
                 (if (string= f2 "") 
                     (error s)
                     (use-value f2 s))))))
      (init-skelfile file name))))

(defcommand (:skel describe) ()
  (describe
   (if (> *argc* 0)
       (find-skelfile (pathname (car *args*)) :load t)
       (or *skel-project* *skel-user-config* *skel-system-config*))))

(defcommand (:skel inspect) ()
  (sb-ext:enable-debugger)
  (setq *no-exit* t)
  (inspect (or *skel-project* *skel-user-config*)))

(defun call-with-args (action args)
  (with-directory (project-root *skel-project*)
    (if (null args)
        (sk-call *skel-project* action)
        (if-let ((comp (find (car args) (components *skel-project*) :key 'name :test 'string-equal)))
          (apply (symbolicate "SK-" (symbol-name action)) comp (cdr args))
          (mapc (lambda (x)
                  (sk-call *skel-project* (keywordicate (symbol-name action) '- (string-upcase x))))
                args)))))

(defcommand (:skel compile) ()
  (call-with-args :compile *args*))
(defcommand (:skel build) ()
  (call-with-args :build *args*))
(defcommand (:skel update) ()
  (call-with-args :update *args*))
(defcommand (:skel dist) ()
  (call-with-args :dist *args*))
(defcommand (:skel install) ()
  (call-with-args :install *args*))
(defcommand (:skel pack) ()
  (call-with-args :pack *args*))
(defcommand (:skel unpack) ()
  (call-with-args :unpack *args*))
(defcommand (:skel bundle) ()
  (call-with-args :bundle *args*))
(defcommand (:skel unbundle) ()
  (call-with-args :unbundle *args*))
(defcommand (:skel clean) ()
  (call-with-args :clean *args*))
(defcommand (:skel test) ()
  (call-with-args :test *args*))
(defcommand (:skel bench) ()
  (call-with-args :bench *args*))
(defcommand (:skel save) ()
  (call-with-args :save *args*))

(defcommand (:skel show) (&rest args)
  (if args
      (mapc (lambda (x) 
              (lety ((y (string-left-trim ":" x) :type base-string))
                (if (sk-project-slot y nil)
                    (let ((val
                            (slot-value
                             *skel-project*
                             (sb-mop:slot-definition-name
                              (find y
                                    (sb-mop:class-slots (class-of *skel-project*))
                                    :test 'string=
                                    :key (lambda (x) (string-downcase (sb-mop:slot-definition-name x))))))))
                      (if (and (sequencep val) (not (stringp val)))
                          (apply 'fmt-column t (coerce val 'list))
                          (sk-print val)))
                    (log:fatal! "unknown argument: ~A~%" x))))
            args)
      (cond
        ((boundp '*skel-project*)
         (sk-print *skel-project* :exclude (if ast:*keep-ast* '(:phases :rules) '(:phases :rules :ast))))
        ((boundp '*skel-user-config*) (sk-print *skel-user-config*))
        ((boundp '*skel-system-config*) (sk-print *skel-system-config*))
        (t (skel-simple-error "skel not installed")))))

(defcommand (:skel list) ()
  (string-case ((subseq (pop *args*) 0 3))
    ("log" (apply 'sk-log-list *args*))))

(defcommand (:skel id) ()
  (println (octet-vector-to-hex-string (integer-to-octets (id:id *skel-project*)))))

(define-command-type skc-config (&optional cfg) (load-user-skelrc (or cfg (user-skelrc)) nil))

(defcommand (:skel edit) ()
  (let ((file (or (when *args* (pop *args*)) (path *skel-project*))))
    (cli/ed:run-emacsclient (namestring file))))

(defcommand (:skel make) ()
  (let ((sk *skel-project*))
    (with-directory (project-root sk)
      (sb-ext:enable-debugger)
      (if *args*
          (loop for a in *args*
                do (debug!
                    (if-let ((rule (sk-find a sk)))
                      (sk-make sk rule)
                      ;;  TODO 2024-08-23: restart condition here
                      (skel-simple-error "rule not found: ~A" a))))
          (sk-make sk (aref (skel/core/obj::rules sk) 0))))))

(defcommand (:skel status) ()
  (vc:vc-status (vc:vc *skel-project*)))

(defcommand (:skel run) ()
  (sb-ext:enable-debugger)
  (if *args*
      (mapc (lambda (script)
              ;; first check if a script with the same name exists, else check
              ;; for a rule definition
              (if-let ((script (sk-find
                                (pathname-name script)
                                *skel-user-config*)))
                (sk-run script)
                (call-with-args :run (list script))))
            *args*)
      (required-argument 'name)))

(defcommand (:skel new) ()
  (println *args*)
  (println *opts*))

(defcommand (:skel search) ()
  (dolist (a *args*)
    (println (sk-search-project a))))

(defun sk-shell ()
  (trace! "starting skel shell")
  ;; TODO 2025-11-16: consolidate usage of *no-exit* vs *interactive* etc
  (setq *no-exit* t)
  (progn
    (in-package :sk-user)
    (using :cl-user :sb-ext :std-user)
    (println "Welcome to SKEL")
    (cli/linedit:install-repl :wrap-current t :history "/tmp/skel.history" :killring "/tmp/skel.killring")
    (cli/repl:make-toplevel-init
     :package :sk-user
     :userinit (lambda () (or (xdg-config-file :core) 
                              (merge-homedir-pathnames ".config/corerc") 
                              (merge-homedir-pathnames ".corerc"))))))

(defcommand skc-shell () (sk-shell))

(define-cli "skel"
  :version (format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :description "The hackable devtool."
  :kernel (with-commands :skel (command 'show)))
