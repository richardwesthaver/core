;;; cli/clap/proto.lisp --- Clap Protocol

;; 

;;; Code:
(in-package :cli/clap)

;;; Protocol
(defgeneric print-help (self &optional stream)
  (:documentation "Format command SELF as a helpful string."))

(defgeneric print-version (self &optional stream)
  (:documentation "Print the version of SELF."))

(defgeneric print-usage (self &optional stream)
  (:documentation "Format command SELF as a useful string."))

;;; CLI Command
(defkernel cli-command (command)
  ;; name slot is required and must be a string
  ((name :initarg :name :initform (required-argument :name) :accessor name :type string)
   (description :initarg :description :accessor description :type string))
  (:documentation "CLI command class inherited by both the 'main' command which is executed when
a CLI is called without arguments, and all subcommands."))

(defmethod initialize-instance :after ((self cli-command) &key)
  (unless (stringp (name self)) (setf (name self) (format nil "~(~A~)" (name self)))))

(defmethod make-load-form ((obj cli-command) &optional env)
  (make-load-form-saving-slots 
   obj 
   :slot-names '(name opts cmds thunk lock description args)
   :environment env))

(defmethod print-object ((self cli-command) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A :active ~A"
            (name self)
            (function-lambda-list self)
            (equal (name self) cmd::*command*))))

(defmethod print-usage ((self cli-command) &optional stream)
  (with-slots (opts cmds) self
    (format stream "~(~A~)~:[~;*~]~24t~@[~A~]~@[~%~4t:doc ~A~]~@[~{~%~4t~A~^~}~]~@[~{~A~}~]~&"
            (name self)
            (when *cli*
              (equal (string (kernel *cli*)) (string (kernel self))))
            (and (slot-boundp self 'description) (description self))
            (when (fboundp (kernel self))
              (documentation (symbol-function (kernel self)) 'function))
            (unless (sequence:emptyp opts)
              (loop for o across opts collect (with-output-to-string (s) (print-usage o s))))
            (unless (sequence:emptyp cmds)
              (loop for c across cmds collect (with-output-to-string (s) (print-usage c s)))))))

(defmethod print-help ((self cli-command) &optional stream)
  (unless (typep self 'cli)
    (print-usage self stream))
  (let ((opts (opts self))
        (cmds (cmds self)))
    (unless (sequence:emptyp opts)
      (println "options:" stream)
      (loop for o across opts
            do (iprintln (with-output-to-string (s) (print-usage o s)) 2 stream)))
    (terpri stream)
    (unless (sequence:emptyp cmds)
      (println "commands:" stream)
      (loop for c across cmds
            do (iprintln (with-output-to-string (s) (print-usage c s)) 2 stream)))))

(defmethod find-opts ((name string) (self cli-command) &key active recurse)
  (let ((ret))
    (flet ((%find (o obj)
             (when-let ((found (find o (opts obj) :key #'name :test 'equal)))
               (push found ret))))
      (when (and recurse (cmds self))
        (loop for c across (cmds self)
              do (%find name c)))
      (%find name self)
      (when active
        (setf ret (remove-if-not #'lock ret)))
      ret)))

(defun find-short-opts (flag cmd &key recurse)
  "Find and return all CLI-OPTs matching character or string FLAG in CMD.

- recurse :: optionally check nested commands as well."
  (let ((ret))
    (flet ((%find (ch obj)
             (when-let ((found (find (coerce ch 'character) obj 
                                     :key #'name
                                     :test #'opt-string-prefix-eq)))
               (push found ret))))
      (flet ((%recurse-ch (ch vec)
               (loop for c across vec
                     do (%find ch (opts c))))
             (%recurse-str (str vec)
               (loop for c across vec
                     for ch across str
                     do (%find ch (opts c)))))
        (etypecase flag
          (character
           (when recurse (%recurse-ch flag (cmds cmd)))
           (%find flag (opts cmd)))
          (string
           (when recurse (%recurse-str flag (cmds cmd)))
           (%find flag (opts cmd))))
        ret))))

(defmethod call :before ((self cli-command) &rest args)
  (log:trace! "calling command: ~A~@[ with args ~A~]~%" self args))
