;;; lib/cli/repl.lisp --- REPL utils

;; For now we rely on Vindarel's excellent CL-READLINE package, which
;; provides bindings to the GNU Readline library and a nice API.

;; ref: https://github.com/vindarel/cl-readline

;;; Code:
(in-package :cli/repl)
;; *command-char* alias make-repl-fun

;;; Allegro-style REPL
;; this should be used as a light layer on top of the standard Lisp
;; REPL provided by SBCL. Basically whenever we're typing input
;; destined for a Lisp reader this is our best bet.
(defun load-acl-repl ()
  "Load the SB-ACLREPL package, applying changes to the default SBCL
REPL."
  (require 'sb-aclrepl))

;;; Readline-style REPL
;; this is suited for non-Lisp input which should skip the Lisp
;; reader. Input is interpreted as strings and handled by the GNU
;; Readline library via FFI. Features include History, Custom
;; Functions, and Custom Keybinds (not available in ACLREPL above).
(defun input-novelty-check (x y)
  (string/= (trim x)
            (trim y)))

(defun start-rl-repl ()
  "Start a GNU Readline REPL."
  (do ((i 0 (1+ i))
       (input ""))
      ((string= "quit" (trim input)))
    (setf input (readline :prompt (format nil "[~a]> " i)
                          :add-history t
                          :novelty-check #'input-novelty-check))))
