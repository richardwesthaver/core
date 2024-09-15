;;; sbcl.lisp --- SBCL Tools

;; 

;;; Code:
(in-package :cli/tools/sbcl)

(deferror sbcl-error (simple-error error) ())

(defun sbcl-error (fmt &rest args)
  (error 'sbcl-error :format-arguments args :format-control fmt))

(defparameter *sbcl* (or sb-ext:*runtime-pathname* (find-exe "sbcl")))

;; ref: section 3.3.1 of the manual
(defvar *sbcl-runtime-options*
  '(help version core dynamic-space-size control-stack-size tls-limit
    noinform disable-ldb lose-on-corruption merge-core-pages no-merge-core-pages))

(defvar *sbcl-toplevel-options*
  '(sysinit userinit no-sysinit no-userinit disable-debugger noprint script quit non-interactive eval load))

(defun parse-sbcl-option-keys (keys)
  (let ((ret))
    (sb-int:doplist (k v) keys
      (unless (null v)
        (push (format nil "--~A" (string-downcase (symbol-name k))) ret)
        (etypecase v
          (boolean nil)
          (string (push v ret)))))
    (nreverse ret)))

(defun run-sbcl (&rest args)
  (let ((proc (sb-ext:run-program *sbcl* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (sbcl-error "SBCL command failed: ~A ~A" *sbcl* (or args "")))))

(defmacro with-sbcl ((&rest keys) &body body)
  "Convenience macro for running an external SBCL process in its own shell. The
keys are the same as those listed in `sbcl --help` and the BODY is wrapped in
a PROGN and passed to the --eval flag."
  `(run-sbcl ,@(when keys (parse-sbcl-option-keys keys))
             ,@(when body
                 (flatten
                  (mapcar (lambda (x) (list "--eval" (with-output-to-string (s) (prin1 x s))))
                          body)))))
