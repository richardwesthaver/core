;;; cli/clap/cli.lisp --- Clap CLI Class

;; Top-level command object of a CLI App

;;; Code:
(in-package :cli/clap/obj)

(defun make-cli (kind &rest slots)
  "Creates a new CLI object of the given kind."
  (declare (type (member :opt :cmd :cli t) kind))
  (cond
    ((eql kind :cli) (apply #'make-instance 'cli slots))
    ((eql kind :opt) (apply #'make-cli-opt slots))
    ((eql kind :cmd) (apply #'make-instance 'cli-cmd slots))
    (t (apply #'make-instance kind slots))))

(defmacro define-cli (sym &key name version description thunk opts cmds)
  "Define a symbol NAME bound to a top-level CLI object."
  (with-gensyms (%name %class)
    (if (atom sym)
        (setq %name sym
              %class :cli)
        (setq %name (car sym)
              %class (cdr sym)))
    `(,*default-cli-def* ,%name (make-cli ,%class :name ,name
                                                  :version ,version
                                                  :description ,description
                                                  :thunk ,thunk
                                                  :opts (make-opts ',opts)
                                                  :cmds (make-cmds ',cmds)))))

(defmacro defmain ((&key (exit t) (export t)) &body body)
  "Define a CLI main function in the current package."
  (let ((main (symbolicate "MAIN")))
    `(let ((*no-exit* ,(not exit)))
       (defun ,main ()
         "Run the top-level function and print to *STDOUT*."
         (with-cli-handlers
           (progn
             ,@body)))
       ,@(when export `((export ',main))))))

;; RESEARCH 2023-09-12: closed over hash-table with short/long flags
;; to avoid conflicts. if not, need something like a flag-function
;; slot at class allocation.
(defun make-opts (opts)
  "Make a vector of CLI-OPTs based on OPTS."
  (map 'vector
       (lambda (x)
         (etypecase x
           (string (make-cli-opt :name x))
           (list (apply #'make-cli :opt x))
           (t (make-cli :opt :name (format nil "~(~A~)" x) :global t))))
       opts))

(defun make-cmds (&rest cmds)
  "Make a vector of CLI-CMDs based on CMDS."
  (map 'vector
        (lambda (x)
          (etypecase x
            (cli-cmd x)
            (string (make-cli :cmd :name x))
            (list (apply #'make-cli :cmd x))
            (t (make-cli :cmd :name (format nil "~(~A~)" x)))))
        cmds))

(defclass cli (cli-cmd)
  ;; name slot defaults to *package*, must be string
  ((name :initarg :name :initform (string-downcase (package-name *package*)) :accessor cli-name :type string)
   (version :initarg :version :initform "0.1.0" :accessor cli-version :type string)
   ;; TODO 2023-10-11: look into pushd popd - cd-stack?
   (cd :initarg :cd :initform (sb-posix:getcwd) :type string :accessor cli-cd
        :documentation "working directory of the top-level CLI."))
  (:documentation "CLI"))

(defmethod print-usage ((self cli) &optional stream)
  (iprintln (format nil "usage: ~A [global] <command> [<arg>]~%" (cli-name self)) 2 stream))

(defmethod print-version ((self cli) &optional stream)
  (println (cli-version self) stream))

(defmethod print-help ((self cli) &optional (stream t)) 
  (println (format nil "~A v~A --- ~A~%" (cli-name self) (cli-version self) (cli-description self)) stream)
  (print-usage self stream)
  ;; (terpri stream)
  (println "options:" stream)
  (with-slots (opts cmds) self
    (unless (null opts)
      (loop for o across opts
            do (iprintln (print-usage o nil) 2 stream)))
    (terpri stream)
    (println "commands:" stream)
    (unless (null cmds)
      (loop for c across cmds
            do (iprintln (print-usage c nil) 2 stream)))))

(defmethod cli-equal :before ((a cli) (b cli))
  "Return T if A is the same cli object as B.

Currently this function is intended only for instances of the CLI
class and is used as a specialized EQL for DEFINE-CONSTANT."
  (with-slots (version) a
    (with-slots ((bv version)) b
      (string= version bv))))

(declaim (inline debug-opts))
(defun debug-opts (cli)
  (let ((o (active-opts cli))
        (a (cli-cmd-args cli))
        (c (active-cmds cli)))
    (log:debug! :pwd (cli-cd cli) :active-opts o :cmd-args a :active-cmds c)))

(defmethod do-opts ((self cli) &optional global)
  (loop for opt across (active-opts self global)
        do (do-opt opt)))

(defmacro with-cli (slots cli &body body)
  "Like with-slots with some extra bindings.

SLOTS is a list passed to WITH-SLOTS.

CLI is updated based on the current environment and dynamically bound to
*CLI*."
  `(progn
     (setq *cli* ,cli)
     (setf (cli-cd ,cli) (sb-posix:getcwd))
     (with-slots ,slots (parse-args ,cli (args) :compile t)
       ,@body)))
