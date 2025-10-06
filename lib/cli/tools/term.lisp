;;; term.lisp --- Terminal Tools

;; Control and spawn terminal consoles from Lisp.

;;; Commentary:

;; This package is intended to make it easier to work with interactive
;; terminal programs in a Lispy manner.

;;; Code:
(in-package :cli/tools/term)

(defparameter *alacritty-config-path* (merge-pathnames ".config/alacritty.toml" (user-homedir-pathname)))

(define-cli-tool :alacritty)
(define-cli-tool :xterm)

(defparameter *term* (or *alacritty* *xterm*))

(deferror term-error (simple-error error) ())

(defconfig term-config (ast) ())

(defconfig alacritty-config (term-config toml-document) 
  ((path :initarg :path :initform *alacritty-config-path* :accessor path)))

(defun load-alacritty-config (&optional (path *alacritty-config-path*))
  (change-class
   (deserialize path :toml)
   'alacritty-config
   :path path))

(defmethod make-config ((self (eql :alacritty)) &key (path *alacritty-config-path*))
  (load-alacritty-config path))

(defun term-error (fmt &rest args)
  (error 'term-error :format-arguments args :format-control fmt))

(defun run-term (&rest args)
  (apply #'sb-ext:run-program *term* args))

(defmacro with-term ((sym &key args input output) &body body)
  `(let ((,sym (run-term ,args
                             ,@(when input '(:input :stream))
                             ,@(when output '(:output :stream))
                             :wait nil)))
     (let (,@(when input `((,input (sb-ext:process-input ,sym))))
           ,@(when output `((,output (sb-ext:process-output ,sym)))))
       ,@body)))

;;; Terminal Recording
(define-cli-tool :script (&rest args)
  (let ((proc (sb-ext:run-program *script* (or args nil) :output t)))
    (unless (not (eq 0 (sb-ext:process-exit-code proc)))
      (script-error "script command failed: ~A " args))))
  
(define-cli-tool :scriptreplay (&rest args)
  (let ((proc (sb-ext:run-program *scriptreplay* (or args nil) :output t)))
    (unless (not (eq 0 (sb-ext:process-exit-code proc)))
      (scriptreplay-error "scriptreplay command failed: ~A " args))))

;;; fbterm
(define-cli-tool :fbterm (&rest args)
  (let ((proc (sb-ext:run-program *fbterm* (or args nil) :output t)))
    (unless (not (eq 0 (sb-ext:process-exit-code proc)))
      (fbterm-error "FBTERM command failed: ~A ~A" *fbterm* args))))
