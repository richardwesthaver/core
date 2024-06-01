(defpackage :vc/tests
  (:use :cl :std :rt :vc/proto :vc/git :vc/hg :vc))

(in-package :vc/tests)
(defsuite :vc)
(in-suite :vc)

(defmacro with-temp-repo ((kind &rest opts) &body body)
  (declare (ignore opts)) ;; TODO 2024-06-01: 
  `(let ((repo ,(make-repo ".")))
     (setf (vc-repo-path repo) (merge-pathnames (format nil "~A" (gensym "repo")) "/tmp/"))
     (case ,kind
       (:hg (sb-mop::change-class repo 'hg-repo))
       (:git (sb-mop::change-class repo 'git-repo))
       (t nil))
     (vc-init repo)
     (let ((*default-pathname-defaults* (vc-repo-path repo)))
       ,@body)))

(deftest git ()
  (with-temp-repo (:git)
    (is (streamp (sb-ext:process-output (run-git-command "status" nil :stream))))))

(deftest hg ()
  (with-temp-repo (:hg)
    (is (streamp (sb-ext:process-output (run-hg-command "status" nil :stream))))))

(deftest vc ()
  (with-temp-repo (*default-vc-kind*) (is repo)))

(defun %mirror-update (path) (declare (ignore path)))

(deftest mirror-network (:disabled t)
  (macrolet ((with-job ((job &rest opts) &body body)
               `(let (()) ,@body)))
    (labels ((%m (name thunk args)))
      (%m "test" #'vc-pull nil))))
