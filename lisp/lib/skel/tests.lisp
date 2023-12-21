;;; tests.lisp --- skel tests
(defpackage :skel/tests
  (:use :cl :skel/core :skel/comp :rt :sxp :log :obj/id)
  (:import-from :uiop :file-exists-p))

(in-package :skel/tests)

(defsuite :skel)
(in-suite :skel)

(defvar %tmp)
(defun tmp-path (ext)
  (setq %tmp (format nil "/tmp/~A.~A" (gensym) ext)))

(defun do-tmp-path (file &rest body)
  (prog1 body
    (when (file-exists-p file) (delete-file file))))

(defun skels (c)
  (let ((s))
    (loop for i from 1 to c
	  do (push (id (make-instance 'sk-project :name (gensym))) s))
    s))

(deftest sanity ()
  "IDs should be reasonably unique."
  (is (eq t (apply #'/= (skels 1000)))))

(deftest header-comments ()
  "Make sure header comments are generated correctly. 

This covers variations of make-source-header-comment, make-source-file-header,
make-shebang-comment, and make-shebang-file-header."
  (is (eq (type-of (make-shebang-file-header 
		    (make-shebang-comment "/dev/null"))) 
	  'file-header))
  (is (eq (type-of (make-source-file-header 
		    (make-source-header-comment 
		     "foo-test"
		     :timestamp t
		     :description "nothing to see here"
		     :opts '("Definitely-Not_Emacs: T;"))))
	  'file-header)))

(deftest skelfile ()
  "Ensure skelfiles are created and loaded correctly and that they signal
the appropriate restarts."
  (do-tmp-path (tmp-path "sk")
    (is (sk-write-file (make-instance 'sk-project :name "nada" :path %tmp) :path %tmp :if-exists :supersede))
    (ignore-errors (delete-file %tmp))
    (setf %tmp (tmp-path "sk"))
    (is (init-skelfile %tmp))
    (is (load-skelfile %tmp))
    (is (build-ast (sk-read-file (make-instance 'sk-project) %tmp)))))

(deftest skelrc ()
  "Ensure skelrc files are created and loaded correctly."
  (do-tmp-path (tmp-path "skrc")))

(deftest makefile ()
  "Make sure makefiles are making out ok."
    (do-tmp-path (tmp-path "mk")
      (flet ((mk (&optional path) (make-instance 'makefile :name (gensym)
							   :path (or path %tmp) :description "barfood"))
	     (src (path) (make-instance 'sk-source :path path))
	     (cmd (body) (make-instance 'sk-command :body body))
	     (rule (tr sr) (make-sk-rule tr sr)))
	(is (null (sk-write-file (mk) :if-exists :supersede :path (tmp-path "mk"))))
	(let* ((tr1 (tmp-path "t1"))
	       (tr2 (tmp-path "t2"))
	       (sr (src (tmp-path "s1")))
	       (r1 (rule tr1 sr))
	       (r2 (rule sr tr2))
	       (mk1 (mk "test.mk")))
	  (is (push-rule r1 mk1))
	  (is (push-rule r2 mk1))
	  ;; NOTE: not really useful yet
	  ;; (is (push-rule r2 mk1 t))
	  ;; (is (push-rule r1 mk1 t))
	  (is (push-directive 
	       (cmd "ifeq ($(DEBUG),1) echo foo 
endif")
	       mk1))
	  ;; (is (push-directive (cmd "") mk1))
	  (is (push-var '(a b) mk1))
	  (is (push-var '(b c) mk1))
	  ;; FIXME
	  ;; (is (null (sk-write-file mk1 :if-exists :supersede :path (tmp-path "mk"))))
	  ))))

(deftest vm ()
  "EXPERIMENTAL"
  (is (let ((vm (make-sk-vm 201)))
	(dotimes (i 200)
	  (sks-pop vm))
	t))
  (let ((vm (make-sk-vm 1)))
    (is (sks-pop vm))
    (signals simple-error (sks-pop vm))))
