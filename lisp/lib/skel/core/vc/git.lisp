;;; Git
(in-package :skel/core)

(define-condition git-error (vc-error) ())

(defvar *git-program* (find-exe "git"))

(defun run-git-command (cmd &rest args)
  (run-program *git-program* (push cmd args) :output :stream))

(defun git-url-p (url)
  "Return nil if URL does not look like a URL to a git valid remote."
  (let ((url-str (if (typep url 'pathname)
                     (namestring url)
                     url)))
    (scan '(:alternation
            (:regex "\\.git$")
            (:regex "^git://")
            (:regex "^https://git\\.")
            (:regex "^git@"))
          url-str)))

(defclass git-repo (repo)
  ((index))) ;; working-directory

(defmethod vc-init ((self git-repo))
  (with-slots (path) self
    (sb-ext:process-exit-code (run-git-command "init" path))))

(defmethod vc-run ((self git-repo) (cmd string) &rest args)
  (with-slots (path) self
    (uiop:with-current-directory (path)
      (with-open-stream (s (sb-ext:process-output (apply #'run-git-command cmd args)))
        (with-output-to-string (str)
          (loop for l = (read-line s nil nil)
                while l
                do (write-line l)))))))

(defmethod vc-init ((self git-repo))
  (with-slots (path) self
    ;; could throw error here but w/e
    (sb-ext:process-exit-code (run-git-command "init" path))))

(defmethod vc-clone ((self git-repo) remote &key &allow-other-keys)
  (with-slots (path) self
    (sb-ext:process-exit-code (run-git-command "clone" remote path))))

(defmethod vc-pull ((self git-repo) remote &key &allow-other-keys)
  (with-slots (path) self
    (with-current-directory (path)
      (sb-ext:process-exit-code (run-git-command "pull" remote)))))

(defmethod vc-push ((self git-repo) remote &key &allow-other-keys)
  (with-slots (path) self
    (with-current-directory (path)
      (sb-ext:process-exit-code (run-git-command "push" remote)))))

(defmethod vc-commit ((self git-repo) msg &key &allow-other-keys)
  (with-slots (path) self
    (with-current-directory (path)
      (sb-ext:process-exit-code (run-git-command "commit" "-m" msg)))))

(defmethod vc-add ((self git-repo) &rest files)
  (with-slots (path) self
    (with-current-directory (path)
      (sb-ext:process-exit-code (apply #'run-git-command "add" files)))))

(defmethod vc-remove ((self git-repo) &rest files)
  (with-slots (path) self
    (with-current-directory (path)
      (sb-ext:process-exit-code (apply #'run-git-command "remove" files)))))

;; TODO
(defmethod vc-addremove ((self git-repo) &rest files)
  (with-slots (path) self
    (with-current-directory (path)
      (sb-ext:process-exit-code (apply #'run-git-command "addremove" files)))))

(defmethod vc-status ((self git-repo) &key &allow-other-keys) (vc-run self "status"))

(defmethod vc-branch ((self git-repo) &key cmd branch &allow-other-keys) (vc-run self "branch" cmd branch))

(defmethod vc-diff ((a git-repo) (b git-repo) &key ediff &allow-other-keys) 
  (vc-run a "diff" (vc-repo-head a) (vc-repo-head b)))

(defmethod vc-id ((self git-repo))
  (with-slots (path) self
    (with-current-directory (path)
      (with-open-stream (s (sb-ext:process-output (run-git-command "id")))
        (with-output-to-string (str)
          (loop for c = (read-char s nil nil)
                while c
                do (write-char c str))
          str)))))

;; TODO 2023-12-29: does git have a cmdserver?
;; (declaim (inline make-git-client))
;; (defstruct git-client
;;   (pid 0 :type fixnum :read-only t)
;;   (pgid 0 :type fixnum)
;;   (cwd (sb-posix:getcwd) :type string))
