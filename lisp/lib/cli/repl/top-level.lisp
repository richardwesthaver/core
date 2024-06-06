(in-package :cli/repl)

;;; TOPLEVEL

;; These macros help with defining a toplevel initialization
;; function. Initialization functions are responsible for parsing runtime
;; options and starting a REPL if needed.
;; (defmacro define-toplevel-init (name (props opts) &body body))
;; (defmacro define-toplevel-repl (name (props opts) &body body))

(defun default-toplevel-init ()
  (let ((opts (cdr *posix-argv*))
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
      (when *posix-argv*
        (setf (cdr *posix-argv*) opts)))))

