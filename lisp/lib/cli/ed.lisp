;;; lib/cli/ed.lisp --- Editor functions

;;

;;; Code:
(in-package :cli/ed)

(defun run-emacs (&optional file args)
  (sb-ext:run-program (find-exe "emacs") `(,@(when file (list file)) ,@args)))
  
(defun run-emacsclient (&optional (file ".") (create-frame t))
  (sb-ext:run-program (find-exe "emacsclient")
                      `(,file ,@(when create-frame (list "-c")) "-a=")
                      :wait nil
                      :output nil))

(push #'run-emacsclient sb-ext:*ed-functions*)
(push #'run-emacs sb-ext:*ed-functions*)

;;; Org Protocol
;; ref: https://orgmode.org/worg/org-contrib/org-protocol.html

;; On GNU/Linux, Emacs is now the default application for
;; 'org-protocol'. (startup change in Emacs 30.1)
(defun org-store-link (url title)
  (run-emacsclient (format nil "org-protocol://store-link?url=~a&title=~a"
                           url title)))

;;; Macros
