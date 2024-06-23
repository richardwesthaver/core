;;; cli/tmux.lisp --- Tmux Tools

;; Control Tmux from Lisp

;;; Commentary:

;; ref: https://github.com/tmux/tmux/wiki/Getting-Started#getting-started

;;; Code:
(in-package :cli/tools/tmux)

(defparameter *tmux-config-path* (merge-pathnames ".tmux.conf" (user-homedir-pathname)))

(defstruct tmux-session)
(defstruct tmux-window)
(defstruct tmux-pane)

(defun run-tmux (&rest args)
  (apply #'sb-ext:run-program (find-exe "tmux") (or args (list nil))))

(defun spawn-tmux (&rest args)
  (run-terminal (append (list "-e" "tmux") args)))

;; (spawn-tmux "a")
