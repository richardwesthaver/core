;;; cli/tools/cc.lisp --- C Compilers

;; Use C Compiler tooling from Lisp.

;;; Commentary:

;; 

;;; Code:
(in-package :cli/tools/cc)

(deferror cc-error (simple-error) () (:auto t))

(defparameter *cc* (find-exe "cc"))

(defparameter *ld*
  (or
   #+unix (find-exe "ld.lld")
   #+darwin (find-exe "ld64.lld")
   #+windows (find-exe "lld-link")
   (find-exe "ld")))

(defun run-cc (&rest args)
  (let ((proc (sb-ext:run-program *cc* args :wait t :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (cc-error "CC command failed: ~A ~A" *cc* (or args "")))))

(defun run-ld (&rest args)
  (let ((proc (sb-ext:run-program *ld* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (cc-error "LD command failed: ~A ~A" *ld* (or args "")))))
