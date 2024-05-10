(defpackage :vc/tests
  (:use :cl :rt :vc))

(in-package :vc/tests)
(defsuite :vc)
(in-suite :vc)
(defmacro with-temp-repo (kind &body body)
  `(let ((repo ,(make-instance 'vc::vc-repo)))
     (setf (vc::vc-repo-path repo) (merge-pathnames (format nil "~A" (gensym "repo")) "/tmp/"))
     (case ,kind
       (:hg (sb-mop::change-class repo 'hg-repo))
       (:git (sb-mop::change-class repo 'git-repo))
       (t nil))
     (vc-init repo)
     (let ((*default-pathname-defaults* (vc::vc-repo-path repo)))
       ,@body)))

(deftest git ()
  (with-temp-repo :git
    (is (streamp (sb-ext:process-output (run-git-command "status" nil :stream))))))

(deftest hg ()
  (with-temp-repo :hg
    (is (streamp (sb-ext:process-output (run-hg-command "status" nil :stream))))))
