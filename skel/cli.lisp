;;; cli.lisp --- Skel CLI Definitions

;; CLI implementation of Skel

;;; Code:
(in-package :skel/cli)
(init :commands :name :skel)

(define-command-type rule (&optional 
                           (prompt "Rule: ") 
                           (completions (map 'list #'sink (skel/core::rules *project*)))
                           (default :error))
  (let ((*query-io* (if (streamp *command-io*) *command-io* *query-io*)))
    (cli/tui:completing-read prompt completions default)))

(define-command-type rule* (&optional (prompt "Rules: ")
                                      (completions (map 'list #'sink (skel/core::rules *project*))))
  (loop for x = (cli/tui:completing-read prompt completions nil)
        while x
        collect x))

;; TODO 2026-05-03: 
(defcommand (:skel init) (&optional file name)
  "Intialize a SKEL-PROJECT."
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

(defcommand (:skel inspect) ()
  "Inspect the current project."
  (setq *interactive* t)
  (inspect (or *project* *project-config*)))

(flet ((call-with-args (action args)
         (with-directory (project-root *project*)
           (if (null args)
               (call *project* action)
               (if-let ((comp (find (car args) (components *project*) :key 'name :test 'string-equal)))
                 (apply (symbolicate "SK-" (symbol-name action)) comp (cdr args))
                 (mapc (lambda (x)
                         (call *project* (keywordicate (symbol-name action) '- (string-upcase x))))
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
                (if-let ((script (project-find
                                  (pathname-name script)
                                  *skel-user-config*)))
                  (exec script)
                  (call-with-args :run (list script))))
              args)
        (required-argument 'name))))

(defcommand (:skel show) (&rest args)
  "Print project or configuration values."
  (declare (interactive *))
  (if args
      (mapc (lambda (x) 
              (let ((y (string-left-trim ":" x)))
                (if (project-slot y :package :skel/core :default nil)
                    (let ((val
                            (slot-value
                             *project*
                             (sb-mop:slot-definition-name
                              (find y
                                    (sb-mop:class-slots (class-of *project*))
                                    :test 'string=
                                    :key (lambda (x) (string-downcase (sb-mop:slot-definition-name x))))))))
                      (if (and (sequencep val) (not (stringp val)))
                          (apply 'fmt-column t (coerce val 'list))
                          (format t "~A~%" val)))
                    (log:fatal! "unknown argument: ~A~%" x))))
            args)
      (cond
        ((boundp '*project*)
         (print-skel-object *project* :exclude (if ast:*keep-ast* '(:phases :rules) '(:phases :rules :ast))))
        ((boundp '*project-config*) (print-skel-object *project-config*))
        (t (skel-simple-error "skel not installed"))))
  (values))

(defcommand (:skel id) ()
  "Print the current project ID as a hexstring and exit."
  (println (octet-vector-to-hex-string (integer-to-octets (id:id *project*)))))

(defcommand (:skel edit) (&optional arg)
  "Edit a project file using ED."
  (let ((file (or arg (path *project*))))
    (ed (namestring file))))

(defcommand (:skel make) (&rest args)
  "Make project rules."
  (declare (interactive (ustring* "Rules: ")))
  (let ((sk *project*))
    (with-directory (project-root sk)
      (if args
          (loop for a in args
                do (debug!
                    (if-let ((rule (project-find a sk)))
                      (make sk rule)
                      ;;  TODO 2024-08-23: restart condition here
                      (skel-simple-error "rule not found: ~A" a))))
          (make sk (aref (skel/core::rules sk) 0))))))

(defcommand (:skel status) ()
  "Print the VC status of the current project."
  (vc:vc-status (vc:vc *project*)))

(defcommand (:skel search) (&rest args)
  "Search the current project and print results."
  (dolist (a args)
    (println (search-project a))))

(defcommand (:skel shell) ()
  "Start the interactive skel REPL."
  (trace! "starting skel shell")
  (setq *interactive* t)
  (progn
    (in-package :sk-user)
    (using :cl-user :sb-ext :std-user)
    (println "Welcome to SKEL")
    (cli/linedit:install-repl 
     :wrap-current t 
     :history (xdg-data-dir :skel "history") 
     :killring (xdg-data-dir :skel "killring"))
    (cli/shell:make-toplevel-init
     :package :sk-user
     :userinit (lambda () (or (xdg-config-file :core) 
                              (merge-homedir-pathnames ".config/corerc") 
                              (merge-homedir-pathnames ".corerc"))))))

(defmain start-skel (:package :sk-user :readtable :shell :commands :skel :cli :skel)
  (init :skel)
  (if-let ((args (cli-args)))
    (destructuring-bind (a1 &rest a2) args
      (if (command a1)
          (if a2 (call-interactively a1 a2) (exec a1))
          (call "show" (cdr *posix-argv*))))
    (call "show" nil)))

(define-cli "skel" #'start-skel
  :version (format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :description "A universal project development tool.")

(save :commands :skel)
