;;; cli/clap/simple.lisp --- Clap Simple

;; 

;;; Code:
(in-package :cli/clap/simple)

;; TODO this is intended to be a simplified functional argument parser
;; which is completely compatible with the toplevel SBCL options.

;; Instead of consuming the args into an AST, we loop over command
;; line options in a lexical context, binding individual symbols.

(defun namestring-to-opt (str) (sb-int:symbolicate (string-upcase (trim str :char-bag '(#\-)))))

(defvar *default-opt-handlers*
  (map 'list
       (lambda (o) (cons (namestring-to-opt o) #'set))
       sb-impl::+runtime-options+))

;; TODO 2024-03-19: need a way to terminate the loop early. (throw/catch)

;; do handlers need to be able to set multiple symbols?

;; should we define opts as special symbols in their own package? (defpackage :OPTS)
(defvar *opt-handlers* *default-opt-handlers*)

(defun find-opt-handler (str)
  (find (namestring-to-opt str) *opt-handlers* :key #'car))

(defmacro with-opts-handled (&body body)
  (let* ((syms (mapcar #'car *opt-handlers*)))
    `(let ((opts (cdr *posix-argv*))
           ,@(mapcar #'list syms))
       (declare (type list opts))
       (flet (($pop ()
                (if opts
                    (pop opts)
                    (sb-impl::startup-error "unexpected end of cli opts"))))
         (loop while opts do
                  (if-let ((opt (find-opt-handler (car opts))))
                    (apply (cdr opt) (car opt) ($pop))))
         (when *posix-argv*
           (setf (cdr *posix-argv*) opts))
         ,@body))))
