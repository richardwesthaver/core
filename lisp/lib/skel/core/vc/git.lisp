;;; Git
(in-package :skel/core)

(define-condition git-error (vc-error)
  ((description :initarg :description :initform nil :reader description))
  (:report (lambda (condition stream)
             (format stream "Git failed: ~a" (description condition)))))

(defvar *git-program* (find-exe "git"))
(defun run-git-command (cmd &rest args)
  (run-program *git-program* (push cmd args)))

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

(declaim (inline make-git-client))
(defstruct git-client
  (pid 0 :type fixnum :read-only t)
  (pgid 0 :type fixnum)
  (cwd (sb-posix:getcwd) :type string))
