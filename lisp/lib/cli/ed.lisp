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

;;; Org Protocol
;; ref: https://orgmode.org/worg/org-contrib/org-protocol.html

;; On GNU/Linux, Emacs is now the default application for
;; 'org-protocol'. (startup change in Emacs 30.1)
(defun org-store-link (url title)
  (run-emacsclient (format nil "org-protocol://store-link?url=~a&title=~a"
                           url title)))
