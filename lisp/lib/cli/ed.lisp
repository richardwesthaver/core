;;; lib/cli/ed.lisp --- Editor functions

;;

;;; Code:
(in-package :cli/ed)

(defun run-emacs (&optional x)
  (run-program (cli:find-exe "emacs") `(,x) :wait nil :output nil))

(defun run-emacsclient (&optional x)
  (run-program (cli:find-exe "emacsclient") `(,x) :wait nil :output nil))

(push #'run-emacs sb-ext:*ed-functions*)
(push #'run-emacsclient sb-ext:*ed-functions*)
