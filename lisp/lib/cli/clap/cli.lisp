;;; cli.lisp --- Clap CLI Class

;; Top-level command object of a CLI App

;;; Code:
(in-package :cli/clap)

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

(defmethod print-help ((self cli) &optional stream) 
  (println (format nil "~A v~A --- ~A~%" (cli-name self) (cli-version self) (cli-description self)) stream)
  (print-usage self stream)
  ;; (terpri stream)
  (println "options:" stream)
  (with-slots (opts cmds) self
    (unless (null opts)
      (loop for o across opts
            do (iprintln (print-usage o) 2 stream)))
    (terpri stream)
    (println "commands:" stream)
    (unless (null cmds)
      (loop for c across cmds
            do (iprintln (print-usage c) 2 stream)))))

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
    (log:debug! (cli-cd cli) o a c)))
