;;; cli/tools/cc.lisp --- C Compilers

;; Use C* Compiler tooling from Lisp.

;;; Commentary:

;; 

;;; Code:
(in-package :cli/tools/cc)

(define-cli-tool :cc (&rest args)
  (let ((proc (sb-ext:run-program *cc* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (cc-error "CC command failed: ~A ~A" *cc* (or args "")))))

(deferror ld-error (simple-error) () (:auto t))

(defparameter *ld*
  (or
   #+unix (find-exe "ld.lld")
   #+darwin (find-exe "ld64.lld")
   #+windows (find-exe "lld-link")
   (find-exe "ld")))

(when *ld* (pushnew :ld *cli-tools*))

(defun run-ld (&rest args)
  (let ((proc (sb-ext:run-program *ld* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (ld-error "LD command failed: ~A ~A" *ld* (or args "")))))

;;; NVCC
(define-cli-tool :nvcc (&rest args)
  (let ((proc (sb-ext:run-program *nvcc* (or args nil) :output t)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (nvcc-error "NVCC command failed: ~A ~@[~A~]" *nvcc* args))))

(define-cli-tool :gdb (args &key (output t) (wait t) input)
  (let ((proc (sb-ext:run-program *gdb* (or args nil) :output output :wait wait :input input)))
    (if (eq 0 (sb-ext:process-exit-code proc))
	nil
	(gdb-error "GDB command failed: ~A ~@[~A~]" *gdb* args))))

(define-cli-tool :lldb (args &key (output t) (wait t) input)
  (let ((proc (sb-ext:run-program *lldb* (or args nil) :output output :wait wait :input input)))
    (if (eq 0 (sb-ext:process-exit-code proc))
	nil
	(lldb-error "LLDB command failed: ~A ~@[~A~]" *lldb* args))))
