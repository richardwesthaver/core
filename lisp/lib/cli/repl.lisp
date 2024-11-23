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

;;; Readline-style REPL
;; this is suited for non-Lisp input which should skip the Lisp
;; reader. Input is interpreted as strings and handled by the GNU
;; Readline library via FFI. Features include History, Custom
;; Functions, and Custom Keybinds (not available in ACLREPL above).
(defun input-novelty-check (x y)
  (string/= (trim x)
            (trim y)))

#+readline
(defun start-rl-repl ()
  "Start a GNU Readline REPL."
  (do ((i 0 (1+ i))
       (input ""))
      ((string= "quit" (trim input)))
    (setf input (readline :prompt (format nil "[~a]> " i)
                          :add-history t
                          :novelty-check #'input-novelty-check))))

;;; TOPLEVEL

;; These macros help with defining a toplevel initialization
;; function. Initialization functions are responsible for parsing runtime
;; options and starting a REPL if needed.
;; (defmacro define-toplevel-init (name (props opts) &body body))
;; (defmacro define-toplevel-repl (name (props opts) &body body))

(defun default-toplevel-init ()
  "Default toplevel initializer - same as the SBCL repl."
  (let ((opts (cdr sb-ext:*posix-argv*))
        (sysinit))
    (declare (type list opts))
    (flet (($pop ()
             (if opts
                 (pop opts)
                 (sb-impl::startup-error "unexpected end of cli opts"))))
      (loop while opts do
               (let ((opt (car opts)))
                 (cond
                   ((string= opt "--sysinit")
                    ($pop)
                    (if sysinit
                        (sb-impl::startup-error "multiple --sysinit opts")
                        (setf sysinit ($pop))))
                   (t
                    (if (find "--end-toplevel-options" opts
                              :test #'string=)
                        (sb-impl::startup-error "bad toplevel opt: ~S"
                                                (car opts))
                        (return))))))
      (when sb-ext:*posix-argv*
        (setf (cdr sb-ext:*posix-argv*) opts)))))

