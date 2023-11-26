;;; tests.lisp --- macs system tests

;;; Commentary:

;; TODO: fix false positives when using (eval-test)

;;; Code:
(uiop:define-package :std/tests
  (:use :cl :std/all :std/rt)
  (:use-reexport :std/tests/sxp))
   
(in-package :std/tests)

(in-readtable :std)

;;; READTABLES
(defsuite :named-readtables)
(in-suite :named-readtables)
(deftest readtables ()
  "Test :std readtable without cl-ppcre"
  (is (typep #`(,a1 ,a1 ',a1 ,@a1) 'function))
  (is (string= #"test "foo" "# "test \"foo\" "))
  (is (string= #$test "1 2 3"$# "test \"1 2 3\"")))

;;; STD
(defsuite :std)
(in-suite :std)

(deftest sym ()
  "Test STD.SYM"
  ;; gensyms
  (is (not (equalp (make-gensym 'a) (make-gensym 'a))))
  (is (eq (ensure-symbol 'tests :std/tests) 'tests))
  (is (eq 'std/tests::foo (format-symbol :std/tests "~A" 'foo)))
  (is (eq (make-keyword 'fizz) :fizz)))

;;;; TODO
(deftest str ()
  "Test STD.STR"
  (is (typep "test" 'string-designator))
  (is (typep 'test 'string-designator))
  (is (typep #\C 'string-designator))
  (is (not (typep 0 'string-designator))))

(deftest list ()
  "Test STD.LIST"
  ;; same object - a literal
  (is (eq (ensure-car '(0)) (ensure-car 0)))
  (is (eq (ensure-car '(nil)) (ensure-car nil)))
  ;; different objects
  (is (not (eq (ensure-cons 0) (ensure-cons 0))))
  (is (equal (ensure-cons 0) (ensure-cons 0))))

(deftest log ()
  "Test STD.LONG"
  (is (debug! "test" *log-level*)))

(deftest cond ()
  "Test STD.COND")

(deftest thread ()
  "Test STD.THREAD"
  (is (stringp (print-thread-info nil))))

(deftest alien ()
  "Test STD.ALIEN"
  (is (= 0 (foreign-int-to-integer 0 4)))
  (is (= 1 (bool-to-foreign-int t))))

(deftest fmt ()
  "Test STD.FMT"
  (is (string= (format nil "| 1 | 2 | 3 |~%") (fmt-row '(1 2 3))))
  (is (string= (fmt-sxhash (sxhash t)) (fmt-sxhash (sxhash t))))
  (is (string= 
       ;; note the read-time-eval..
       #.(fmt-tree nil '(foobar (:a) (:b) (c) (d)) :layout :down)
       #"FOOBAR
 ├─ :A
 ├─ :B
 ├─  C
 ╰─  D
"#))
;; with plist option
  (is (string= 
       #.(std/fmt:fmt-tree nil '(sk-project :name "foobar" :path "/a/b/c.asd" :vc :hg) :layout :down :plist t)
       #"SK-PROJECT
 ├─ :NAME
 │   ╰─ "foobar"
 ├─ :PATH
 │   ╰─ "/a/b/c.asd"
 ╰─ :VC
     ╰─ :HG
"#)))

(deftest ana ()
  "Test STD.ANA"
  (is (= 8 
	 (aif (+ 2 2)
	      (+ it it)))))

(deftest pan ()
  "Test STD.PAN"
  (let ((p
	  (plambda (a) (b c)
		   (if (not a)
		       (setq b 0
			     c 0)
		       (progn (incf b a) (incf c a))))))
    (with-pandoric (b c) p
      (is (= 0 (funcall p nil)))
      (is (= 1 (funcall p 1)))
      (is (= 1 b c)))))

;;; RT
(defsuite :rt)
(in-suite :rt)
(deftest rt (:bench 100 :profile t :persist nil)
  (is (typep (make-fixture-prototype :empty nil) 'fixture-prototype))
  (with-fixture (fx (make-fixture ((a 1) (b 2)) 
		      (:+ () (+ (incf a) (incf b)))
		      (:- () (- (decf a) (decf b)))
		      (t () 0)))
    (is (= 5 (funcall fx :+)))
    (is (= 7 (funcall fx :+)))
    (is (= 5 (funcall fx :-)))
    (is (= 0 (funcall fx))))
  (signals (error t) (test-form (make-instance 'test-result))))

;;; CLI
(defsuite :cli)
(in-suite :cli)
(unless *compile-tests*
  (deftest cli-prompt ()
    "Test MACS.CLI prompts"
    (make-prompt! tpfoo "testing: ")
    (defvar tcoll nil)
    (defvar thist nil)
    (let ((*standard-input* (make-string-input-stream 
			     (format nil "~A~%~A~%" "foobar" "foobar"))))
      ;; prompts 
      (is (string= (tpfoo-prompt) "foobar"))
      (is (string= "foobar"
		   (cli:completing-read "nothing: " tcoll :history thist :default "foobar"))))))

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

