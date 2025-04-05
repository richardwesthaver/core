;;; clap.lisp --- CLAP tests

;; 

;;; Code:
(in-package :cli/tests)
(in-suite :cli)

(setf *no-exit* t)

(defcmd flub-thunk ()
  ;; FIX 2024-10-01: 
  (println *optc*)
  (println *argc*)
  (print *opts*)
  (print *args*))

(defparameter *test-opts* '((:name "foo" :description "bar" :kind string)
                            (:name "bar" :description "foo" :kind string)))
(defparameter *cmd1* (make-cli :cmd :name "holla" :opts *test-opts* :description "cmd1 description"))
(defparameter *cmd2* (make-cli :cmd :name "ayo" :cmds (vector *cmd1*) :opts *test-opts* :description "cmd1 description"))
(defparameter *cmd3* (make-cli :cmd :name "flub" :opts *test-opts* :thunk 'flub-thunk))
(defparameter *cmds* (make-cmds (list `(:name "baz" :description "baz" :opts ,*test-opts*) *cmd1* *cmd2* *cmd3*)))

(defparameter *test-cli* (make-cli :cli :opts *test-opts* :cmds *cmds* :description "test cli"))

(deftest mixed-args ()
  (with-cli (*test-cli* :exit nil) '("--foo" "bar" "flub") 
    (is (string= "bar" (cli-opt-val (aref (opts *cli*) 0))))
    (is (null (cli-args *cli*)))
    (do-cmd *cli*)))

(deftest cli-ast ()
  "Validate the CLI/CLAP/AST parser."
  (is (string= (cli-opt-name (cli-node-form (car (ast:ast (proc-args *test-cli* '("--foo" "1"))))))
               "foo"))
  (signals clap-unknown-argument
    (proc-args *test-cli* '("--log" "default" "--foo=11"))))

(defmain foo-main (:exit nil)
  (with-cli (*test-cli* :exit nil) ()
    t))

(deftest clap-main ()
  (is (null (funcall #'foo-main))))

(deftest clap-basic (:skip t)
  "test basic CLAP functionality."
  (with-cli ((make-cli :cli :opts *test-opts* :cmds *cmds* :description "test cli") opts cmds args :exit nil) *args*
    (is (eq (make-shorty "test") #\t))
    (is (equalp (proc-args *cli* '("-f" "baz" "--bar=fax")) ;; not eql
                (make-cli-ast 
                 (list (make-cli-node 'opt (find-short-opts #\f *cli*))
                       (make-cli-node 'cmd (find-cmd *cli* "baz"))
                       (make-cli-node 'opt (find-opts *cli* "bar"))
                       (make-cli-node 'arg "fax")))))
    (parse-args *cli* '("--bar" "baz" "-f" "yaks")))
    (is (stringp
         (with-output-to-string (s)
           (print-version *cli* s)
           (print-usage *cli* s)
           (print-help *cli* s))))
    (is (string= "foobar" (cli/clap:parse-string-opt "foobar")))
  (do-cmd *cli*))

(make-opt-parser trivial *arg*)

(deftest clap-opts ()
  "CLAP opt tests."
  (is (reduce (lambda (x y) (and x y))
              (loop for k across *cli-opt-kinds* collect (cli-opt-kind-p k))))
  (is (parse-trivial-opt t))
  (is (null (parse-trivial-opt nil))))
