;;; clap.lisp --- CLAP tests

;; 

;;; Code:
(in-package :cli/tests)
(in-suite :cli)

(defcmd flub-thunk
  ;; FIX 2024-10-01: 
  (println *optc*)
  (println *argc*)
  (print *opts*)
  (print *args*))

(defparameter *opts* '((:name "foo" :description "bar" :kind string)
                       (:name "bar" :description "foo" :kind string)))
(defparameter *cmd1* (make-cli :cmd :name "holla" :opts *opts* :description "cmd1 description"))
(defparameter *cmd2* (make-cli :cmd :name "ayo" :cmds (vector *cmd1*) :opts *opts* :description "cmd1 description"))
(defparameter *cmd3* (make-cli :cmd :name "flub" :opts *opts* :thunk 'flub-thunk))
(defparameter *cmds* (make-cmds (list `(:name "baz" :description "baz" :opts ,*opts*) *cmd1* *cmd2* *cmd3*)))

(defparameter *cli* (make-cli :cli :opts *opts* :cmds *cmds* :description "test cli"))

(deftest mixed-args ()
  (with-cli (*cli*) '("--foo" "bar" "flub") 
    (is (string= "bar" (cli-opt-val (aref (opts *cli*) 0))))
    (is (null (cli-args *cli*)))
    (do-cmd *cli*)))

(deftest cli-ast ()
  "Validate the CLI/CLAP/AST parser."
  (is (string= (cli-opt-name (cli-node-form (car (ast (proc-args *cli* '("--foo" "1"))))))
               "foo"))
  (signals clap-unknown-argument
    (proc-args *cli* '("--log" "default" "--foo=11"))))

(defmain foo-main (:exit nil)
  (with-cli (*cli*) ()
    (log:trace! "defmain is OK")
    t))

(deftest clap-main ()
  (is (null (funcall #'foo-main))))

(deftest clap-basic (:skip t)
  "test basic CLAP functionality."
  (with-cli (*cli* opts cmds args) *args*
    (is (eq (make-shorty "test") #\t))
    (is (equalp (proc-args *cli* '("-f" "baz" "--bar=fax")) ;; not eql
                (make-cli-ast 
                 (list (make-cli-node 'opt (find-short-opts *cli* #\f))
                       (make-cli-node 'cmd (find-cmd *cli* "baz"))
                       (make-cli-node 'opt (find-opts *cli* "bar"))
                       (make-cli-node 'arg "fax")))))
    (is (parse-args *cli* '("--bar" "baz" "-f" "yaks")))
    (is (stringp
       (with-output-to-string (s)
         (print-version *cli* s)
         (print-usage *cli* s)
         (print-help *cli* s))))
  (is (string= "foobar" (cli/clap:parse-string-opt "foobar")))
  (do-cmd *cli*)))

(deftest clap-opts ()
  "CLAP opt tests."
  (make-opt-parser trivial *arg*)
  (is (reduce (lambda (x y) (and x y))
              (loop for k across *cli-opt-kinds* collect (cli-opt-kind-p k))))
  (is (parse-trivial-opt t))
  (is (null (parse-trivial-opt nil))))
