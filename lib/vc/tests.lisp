(defpackage :vc/tests
  (:use :cl :std :rt :vc/proto :vc/git :vc/hg :vc :io/zstd :io/flate))

(in-package :vc/tests)
(defsuite :vc)
(in-suite :vc)

(defmacro with-temp-repo (kind &body body)
  `(let ((repo ,(make-repo ".")))
     (setf (path repo) (merge-pathnames (format nil "~A" (gensym "repo")) "/tmp/"))
     (case ,kind
       (:hg (sb-mop::change-class repo 'hg-repo))
       (:git (sb-mop::change-class repo 'git-repo))
       (t nil))
     (vc-init repo)
     (let ((*default-pathname-defaults* (directory-path (path repo))))
       ,@body)))

(deftest git-simple ()
  (with-temp-repo :git
    (is (streamp (sb-ext:process-output (run-git-command "status" nil :stream))))))

(deftest hg-simple ()
  (with-temp-repo :hg
    (is (streamp (sb-ext:process-output (run-hg-command "status" nil :stream))))))

(deftest vc-simple ()
  (with-temp-repo *default-vc-kind* (is repo)))

;; TODO 2024-08-22: 
(deftest vc-mirror-update (:skip :todo)
  "This test replicates a nushell script we've used for a very long time - 'use
vc.nu; vc mirrors update;'"
  (with-temp-repo :hg
    (id:id repo)))

(deftest vc-iterator (:skip :todo)
  "Test iteration over a set of VC-REPOs.")

(deftest vc-bundle ()
  (with-temp-repo :hg
    (close
     (open ".hgignore" :direction :output))
    (vc-add repo ".hgignore")
    (vc-commit repo "dummy commit")
    (let ((out #p"/tmp/bundle.hg.zst"))
      (isequal out (vc-bundle repo out :type "zstd-v2"))
      (delete-file out))))
