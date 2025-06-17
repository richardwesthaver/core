;;; make.lisp --- GNU Make

;; 

;;; Code:
(in-package :cli/tools/make)

(define-cli-tool :make (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *make* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (make-error "MAKE command failed: ~A ~A" *make* (or args "")))))

(define-cli-tool :cmake (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *cmake* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (cmake-error "CMAKE command failed: ~A ~A" *cmake* (or args "")))))
