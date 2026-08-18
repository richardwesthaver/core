;;; skel/tests.lisp --- skel tests

;;; Code:
(defpackage :skel/tests
  (:use :std-lisp :skel :rt :log :obj :skel/packy :skel/krypt :doc)
  (:import-from :uiop :file-exists-p))

(in-package :skel/tests)

(defsuite :skel)
(in-suite :skel)
(load-alien :rocksdb)
(defun %tmp-path (ext) (make-pathname :name (namestring (tmpize-pathname (string (gensym "g")))) :type ext))

(deftest skelfile ()
  "Ensure skelfiles are created and loaded correctly and that they signal
the appropriate restarts."
  (let ((tmp (tmp-path "skelfile")))
    (let ((p (make-instance 'skel-project :name "nada" :path tmp :vc :hg :description "test")))
      (write-ast p tmp)
      (is (load-skelfile tmp))
      (is (build (make-instance 'skel-project :ast (std:file-read-forms tmp)))))))

(deftest skelrc ()
  "Ensure skelrc files are created and loaded correctly."
  (load-skelrc)
  (load-user-skelrc))

(deftest makefile ()
  "Make sure makefiles are making out ok."
    (with-tmp-file (f :name "" :type "mk")
      (flet ((mk (&optional path) (make-instance 'makefile :name (gensym)
                                                 :description "foobar"
							                     :path (or 
                                                        (when path (merge-pathnames path *tmp*))
                                                        *tmp*)))
	         (src (path) (list path))
	         (cmd (&rest body) body)
	         (rule (tr sr) (make-rule (file-namestring tr) sr)))
	    (is (mk (merge-pathnames (%tmp-path "mk") *tmp*)))
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

(deftest packy-db ()
  (let ((*packy-home* "/tmp/packy-test/"))
    (ensure-directories-exist *packy-home*)
    (with-db (db :db (make-db :packy) :open t :close t)
      (is (db-open-p db)))))
