;;; lib/cli/repl.lisp --- REPL utils

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

;;; TOPLEVEL

;; These macros help with defining a toplevel initialization
;; function. Initialization functions are responsible for parsing runtime
;; options and starting a REPL if needed.
;; (defmacro define-toplevel-init (name (props opts) &body body))
;; (defmacro define-toplevel-repl (name (props opts) &body body))

(defun default-toplevel-init (&optional (package *package*))
  "Default toplevel initializer - wraps SBCL init."
  (with-package package
    (sb-impl::toplevel-init)))
