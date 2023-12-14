(defpackage :cli/tests
  (:use :cl :std :rt :cli))

(in-package :cli/tests)
(def-suite :cli)
(in-suite :cli)

(deftest cli-prompt ()
  "Test MACS.CLI prompts"
  (defprompt tpfoo "testing: ")
  (defvar tcoll nil)
  (defvar thist nil)
  (let ((*standard-input* (make-string-input-stream 
			   (format nil "~A~%~A~%" "foobar" "foobar"))))
    ;; prompts 
    (is (string= (tpfoo-prompt) "foobar"))
    (is (string= "foobar"
		 (cli:completing-read "nothing: " tcoll :history thist :default "foobar")))))

(defparameter *opts* (cli:make-opts (:name foo :global t :description "bar")
		       (:name bar :description "foo")))

(defparameter *cmd1* (make-cli :cmd :name "holla" :opts *opts* :description "cmd1 description"))
(defparameter *cmd2* (make-cli :cmd :name "ayo" :cmds #(*cmd1*) :opts *opts* :description "cmd1 description"))
(defparameter *cmds* (cli:make-cmds (:name "baz" :description "baz" :opts *opts*)))

(defparameter *cli* (make-cli t :opts *opts* :cmds *cmds* :description "test cli"))

(deftest cli ()
  "test MACS.CLI OOS."
  (let ((cli *cli*))
    (is (eq (make-shorty "test") #\t))
    (is (equalp (proc-args cli '("-f" "baz" "--bar" "fax")) ;; not eql
		(make-cli-ast 
		 (list (make-cli-node 'opt (find-short-opt cli #\f))
		       (make-cli-node 'cmd (find-cmd cli "baz"))
		       (make-cli-node 'opt (find-opt cli "bar"))
		       (make-cli-node 'arg "fax")))))
    (is (parse-args cli '("--bar" "baz" "-f" "yaks")))
    (is (stringp
	 (with-output-to-string (s)
	   (print-version cli s)
	   (print-usage cli s)
	   (print-help cli s))))
    (is (string= "foobar" (parse-str-opt "foobar")))))
