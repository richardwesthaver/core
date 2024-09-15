(defpackage :vc/tests
  (:use :cl :std :rt :vc/proto :vc/git :vc/hg :vc))

(in-package :vc/tests)
(defsuite :vc)
(in-suite :vc)

(defmacro with-temp-repo (kind &body body)
  `(let ((repo ,(make-repo ".")))
     (setf (vc-path repo) (merge-pathnames (format nil "~A" (gensym "repo")) "/tmp/"))
     (case ,kind
       (:hg (sb-mop::change-class repo 'hg-repo))
       (:git (sb-mop::change-class repo 'git-repo))
       (t nil))
     (vc-init repo)
     (let ((*default-pathname-defaults* (vc-path repo)))
       ,@body)))

(deftest git ()
  (with-temp-repo :git
    (is (streamp (sb-ext:process-output (run-git-command "status" nil :stream))))))

(deftest hg ()
  (with-temp-repo :hg
    (is (streamp (sb-ext:process-output (run-hg-command "status" nil :stream))))))

(deftest vc ()
  (with-temp-repo (*default-vc-kind*) (is repo)))

;; TODO 2024-08-22: 
(deftest vc-mirror-update (:skip t)
  "This test replicates a nushell script we've used for a very long time - 'use
vc.nu; vc mirrors update;'"
  (with-temp-repo :hg
    (vc-id repo)))
