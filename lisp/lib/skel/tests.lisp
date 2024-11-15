;;; skel/tests.lisp --- skel tests
(defpackage :skel/tests
  (:use :cl :skel :rt :log :obj :dat/sxp :std/path)
  (:import-from :uiop :file-exists-p))

(in-package :skel/tests)

(defsuite :skel)
(in-suite :skel)

(defun tmp-path (ext) (make-pathname :name (namestring (tmpize-pathname (string (gensym "g")))) :type ext))

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
  (with-tmp-file (f :type "sk")
    (is (sk-write-file
         (make-instance 'sk-project :name "nada" :path "test" :vc :hg) :path *tmp* :if-exists :supersede))
    (is (load-skelfile *tmp*))
    (is (build-ast (sk-read-file (make-instance 'sk-project) *tmp*)))))

(deftest skelrc ()
  "Ensure skelrc files are created and loaded correctly."
  (with-tmp-file (f :name "" :type "skelrc")))

(deftest makefile ()
  "Make sure makefiles are making out ok."
    (with-tmp-file (f :name "" :type "mk")
      (flet ((mk (&optional path) (make-instance 'makefile :name (gensym)
							   :path (or 
                                                                  (when path (merge-pathnames path *tmp*))
                                                                  *tmp*)
                                                           :description "barfood"))
	     (src (path) (list path))
	     (cmd (&rest body) body)
	     (rule (tr sr) (make-sk-rule (file-namestring tr) sr)))
	(is (null (sk-write-file (mk) :if-exists :supersede :path (merge-pathnames (tmp-path "mk") *tmp*))))
	(let* ((tr1 (tmp-path "t1"))
	       (tr2 (tmp-path "t2"))
	       (sr (src (tmp-path "s1")))
	       (r1 (rule tr1 sr))
	       (r2 (rule (car sr) (src tr2)))
	       (mk1 (mk "test.mk")))
	  (is (push-mk-rule r1 mk1))
	  (is (push-mk-rule r2 mk1))
	  (is (push-mk-directive 
	       (cmd "ifeq ($(DEBUG),1) echo foo 
endif")
	       mk1))
	  (is (push-mk-var '(a b) mk1))
	  (is (push-mk-var '(b c) mk1))
	  ;; FIXME
	  (is 
           (null 
            (sk-write-file mk1 :if-exists :supersede :path (merge-pathnames (tmp-path "mk") *tmp*))))))))

(deftest asd ()
  (let ((sk (make-instance 'sk-project :components '((:lisp "test")
                                                     (:lisp-system "test")))))
    (is sk)))
