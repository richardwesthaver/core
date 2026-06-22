;;; skel/tests.lisp --- skel tests
(defpackage :skel/tests
  (:use :std-lisp :skel :rt :log :obj :skel/packy :skel/krypt :doc)
  (:import-from :uiop :file-exists-p))

(in-package :skel/tests)

(defsuite :skel)
(in-suite :skel)

(defun %tmp-path (ext) (make-pathname :name (namestring (tmpize-pathname (string (gensym "g")))) :type ext))

(deftest skelfile ()
  "Ensure skelfiles are created and loaded correctly and that they signal
the appropriate restarts."
  (with-tmp-file (f :type "sk")
    (let ((p (make-instance 'skel-project :name "nada" :path "test" :vc :hg)))
      (write-ast p *tmp* :if-exists :supersede)
      (is (load-skelfile *tmp*))
      (is (build (apply 'make-instance 'skel-project (std:file-read-forms *tmp*)))))))

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
	         (rule (tr sr) (make-rule (file-namestring tr) sr)))
	    (is (null (ast (mk (merge-pathnames (%tmp-path "mk") *tmp*)))))
	    (let* ((tr1 (%tmp-path "t1"))
	           (tr2 (%tmp-path "t2"))
	           (sr (src (%tmp-path "s1")))
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
	      ;; FIX
	      (is 
           (null 
            (write-ast mk1 (merge-pathnames (%tmp-path "mk") *tmp*) :if-exists :supersede)))))))

(deftest asd ()
  (let ((sk (make-instance 'skel-project :components '((:lisp "test")
                                                       (:lisp-system "test")))))
    (is sk)))

(load-database-backend :packy)

(deftest packy-db ()
  (with-db (db :db (make-db :packy) :open t :close t)
    (is (db-open-p db))))
