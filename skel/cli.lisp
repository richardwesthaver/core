;;; cli.lisp --- Skel CLI Definitions

;; CLI implementation of Skel

;;; Code:
(in-package :skel/cli)

(init :commands :name :skel :copy :cli :clean t)

(defcommand (:skel init) (&optional file name)
  (handler-bind
      ((sb-ext:file-exists
         #'(lambda (s)
             (std:println (format nil "file already exists: ~A" 
                                  (or file *default-skelfile*)))
             (let ((f2 (read-line)))
               (if (string= f2 "") 
                   (error s)
                   (use-value f2 s))))))
    (init-skelfile file name)))

(defcommand (:skel describe) (&optional arg)
  (describe
   (if arg
       (find-skelfile (pathname arg) :load t)
       (or *skel-project* *skel-user-config* *skel-system-config*))))

(defcommand (:skel inspect) ()
  (setq *no-exit* t)
  (inspect (or *skel-project* *skel-user-config*)))

(flet ((call-with-args (action args)
         (with-directory (project-root *skel-project*)
           (if (null args)
               (sk-call *skel-project* action)
               (if-let ((comp (find (car args) (components *skel-project*) :key 'name :test 'string-equal)))
                 (apply (symbolicate "SK-" (symbol-name action)) comp (cdr args))
                 (mapc (lambda (x)
                         (sk-call *skel-project* (keywordicate (symbol-name action) '- (string-upcase x))))
                       args))))))
  (defcommand (:skel compile) (&rest args)
    (call-with-args :compile args))
  (defcommand (:skel build) (&rest args)
    (call-with-args :build args))
  (defcommand (:skel update) (&rest args)
    (call-with-args :update args))
  (defcommand (:skel dist) (&rest args)
    (call-with-args :dist args))
  (defcommand (:skel install) (&rest args)
    (call-with-args :install args))
  (defcommand (:skel pack) (&rest args)
    (call-with-args :pack args))
  (defcommand (:skel unpack) (&rest args)
    (call-with-args :unpack args))
  (defcommand (:skel bundle) (&rest args)
    (call-with-args :bundle args))
  (defcommand (:skel unbundle) (&rest args)
    (call-with-args :unbundle args))
  (defcommand (:skel clean) (&rest args)
    (call-with-args :clean args))
  (defcommand (:skel test) (&rest args)
    (call-with-args :test args))
  (defcommand (:skel bench) (&rest args)
    (call-with-args :bench args))
  (defcommand (:skel save) (&rest args)
    (call-with-args :save args))
  (defcommand (:skel run) (&rest args)
    (if args
        (mapc (lambda (script)
                ;; first check if a script with the same name exists, else check
                ;; for a rule definition
                (if-let ((script (sk-find
                                  (pathname-name script)
                                  *skel-user-config*)))
                  (sk-run script)
                  (call-with-args :run (list script))))
              args)
        (required-argument 'name))))

(defcommand (:skel show) (&rest args)
  (declare (interactive *))
  (if args
      (mapc (lambda (x) 
              (let ((y (string-left-trim ":" x)))
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
                          (format t "~A~%" val)))
                    (log:fatal! "unknown argument: ~A~%" x))))
            args)
      (cond
        ((boundp '*skel-project*)
         (print-skel-object *skel-project* :exclude (if ast:*keep-ast* '(:phases :rules) '(:phases :rules :ast))))
        ((boundp '*skel-user-config*) (print-skel-object *skel-user-config*))
        ((boundp '*skel-system-config*) (print-skel-object  *skel-system-config*))
        (t (skel-simple-error "skel not installed"))))
  (values))

(defcommand (:skel id) ()
  (println (octet-vector-to-hex-string (integer-to-octets (id:id *skel-project*)))))

(defcommand (:skel edit) (&optional arg)
  (let ((file (or arg (path *skel-project*))))
    (cli/ed:run-emacsclient (namestring file))))

(defcommand (:skel make) (&rest args)
  (declare (interactive (ustring* "Make what? ")))
  (let ((sk *skel-project*))
    (with-directory (project-root sk)
      (if args
          (loop for a in args
                do (debug!
                    (if-let ((rule (sk-find a sk)))
                      (sk-make sk rule)
                      ;;  TODO 2024-08-23: restart condition here
                      (skel-simple-error "rule not found: ~A" a))))
          (sk-make sk (aref (skel/core::rules sk) 0))))))

(defcommand (:skel status) ()
  (vc:vc-status (vc:vc *skel-project*)))

(defcommand (:skel search) (&rest args)
  (dolist (a args)
    (println (sk-search-project a))))

(defcommand (:skel shell) ()
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

(defmain start-skel (:package :sk-user :readtable :shell :commands :skel :cli :skel)
  (init :skel)
  (let ((a1 (second *posix-argv*))
        (a2 (cddr *posix-argv*)))
    (if (command a1)
        (if a2 (call-interactively a1 a2) (exec a1))
        (call "show" (cdr *posix-argv*)))))

(define-cli "skel" #'start-skel
  :version (format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :description "The hackable devtool.")

(save :commands :skel)
