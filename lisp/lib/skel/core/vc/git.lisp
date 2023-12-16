;;; Git
(defvar *git-program* (find-exe "git"))
(defun run-git-command (cmd &rest args)
  (run-program *git-program* (push cmd args)))

(defclass git-repo (repo)
  ((index))) ;; working-directory
