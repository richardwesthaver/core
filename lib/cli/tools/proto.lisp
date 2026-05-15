;;; proto.lisp --- CLI Tools Protocol

;; 

;;; Code:
(in-package :cli/tools/proto)

(defconfig cli-tool-config () ())

(defvar *cli-tools* nil)

(define-condition cli-tool-error (simple-error) ())

(defmacro define-cli-tool (name &optional args &body body)
  "Define a new cli tool with a NAME-error condition, a *NAME* variable, and a
run-NAME function.

ARGS and BODY are parsed as the args and body of the run-NAME function."
  (with-gensyms (var err run)
    (let ((%name (string name)))
      (setf 
       var (symbolicate #\* %name #\*)
       err (symbolicate %name "-ERROR")
       run (when args (symbolicate "RUN-" %name)))
      `(eval-always
         (defvar ,var 
           (find-exe ,(etypecase name
                        (string name)
                        (symbol (string-downcase %name)))))
         ,@(when var `((pushnew ,name *cli-tools*)))
         (deferror ,err (cli-tool-error) () (:reporter t))
         ,@(when args `((defun ,run ,args ,@body)))))))

(defun tool-function (n) `(function ,(intern (format nil "RUN-~A" n) :cli/tools)))
(defun tool-path (n) (symbol-value (intern (format nil "*~A*" n) :cli/tools)))
(defun tool-error (n) (find-class (intern (format nil "~A-ERROR" n) :cli/tools)))

(definline find-tool (name)
  (when (memq name *cli-tools*)
    (let ((n (symbol-name name)))
      (values (tool-function n) (tool-path n) (tool-error n)))))
