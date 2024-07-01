;;; term.lisp --- Terminal Tools

;; Control and spawn terminal consoles from Lisp.

;;; Commentary:

;; This package is intended to make it easier to work with interactive
;; terminal programs in a Lispy manner.

;;; Code:
(in-package :cli/tools/term)

(deferror terminal-error (simple-error error) ())

(defun terminal-error (fmt &rest args)
  (error 'terminal-error :format-arguments args :format-control fmt))

(defparameter *terminal* (or (find-exe "alacritty") (find-exe "xterm")))

(defparameter *alacritty-config-path* (merge-pathnames ".config/alacritty.toml" (user-homedir-pathname)))

(defun run-terminal (&rest args)
  (apply #'sb-ext:run-program *terminal* args))

(defmacro with-terminal ((sym &key args input output) &body body)
  `(let ((,sym (run-terminal ,args
                             ,@(when input '(:input :stream))
                             ,@(when output '(:output :stream))
                             :wait nil)))
     (let (,@(when input `((,input (sb-ext:process-input ,sym))))
           ,@(when output `((,output (sb-ext:process-output ,sym)))))
       ,@body)))
