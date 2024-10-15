;;; cli/clap/opt.lisp --- Clap Opts

;; CLI Opt Objects

;;; Code:
(in-package :cli/clap/obj)

;;; Parsers
(make-opt-parser string *arg*)

(make-opt-parser boolean (when *arg* t))

(make-opt-parser (form string) (read-from-string *arg*))

(make-opt-parser (list form) (when (listp *arg*) *arg*))

(make-opt-parser (symbol form) (when (symbolp *arg*) *arg*))

(make-opt-parser (keyword form) (when (keywordp *arg*) *arg*))

(make-opt-parser number (when *arg* (parse-number *arg*)))

(make-opt-parser integer (when *arg* (parse-integer *arg*)))

(make-opt-parser (file string) 
  (parse-native-namestring *arg* nil *default-pathname-defaults* :as-directory nil))

(make-opt-parser (directory string)
  (sb-ext:parse-native-namestring *arg* nil *default-pathname-defaults* :as-directory t))

(make-opt-parser (pathname string)
  (pathname *arg*))

;;; Objects
(defstruct cli-opt
  ;; note that cli-opts can have a nil or unbound name slot
  (name "" :type string)
  (kind 'boolean :type (or symbol list))
  (thunk 'identity :type symbol)
  (val nil)
  (description nil :type (or null string))
  (lock nil :type boolean))

(defmethod cli-name ((self cli-opt))
  (cli-opt-name self))

(defmethod activate-opt ((self cli-opt))
  (setf (cli-opt-lock self) t))

(defmethod cli-lock-p ((self cli-opt))
  (cli-opt-lock self))

(defun %compose-flag-opt (o)
  (activate-opt o)
  (setf (cli-opt-val o) t)
  (make-cli-node 'opt o))

(defun %compose-flag-opts (&rest os)
  (let ((ret))
    (dolist (o os ret)
      (%compose-flag-opt o))))

(defun %compose-value-opt (o &optional val)
  (activate-opt o)
  (setf (cli-opt-val o) val)
  (make-cli-node 'opt o))

(defun %compose-keyword-opt (o val)
  (activate-opt o)
  (setf (cli-opt-val o) val)
  (make-cli-node 'opt o))

(defmethod initialize-instance :after ((self cli-opt) &key)
  (with-slots (name thunk) self
    (unless (stringp name) (setf name (format nil "~(~A~)" name)))
    self))

(defmethod make-load-form ((obj cli-opt) &optional env)
  (make-load-form-saving-slots
   obj
   :slot-names '(name kind thunk val description lock)
   :environment env))

(defmethod install-thunk ((self cli-opt) (lambda function) &optional compile)
  "Install THUNK into the corresponding slot in cli-cmd SELF."
  (let ((%thunk (if compile (compile nil lambda) lambda)))
    (setf (cli-thunk self) %thunk)
    self))

(defmethod print-object ((self cli-opt) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :active ~A :val ~A"
            (cli-opt-name self)
            (cli-opt-lock self)
            (cli-opt-val self))))

(defmethod print-usage ((self cli-opt) &optional stream)
  (format stream "-~(~{~A~^/--~}~) ~A"
          (let ((n (cli-opt-name self)))
            (declare (simple-string n))
            (list (make-shorty n) n))
          (if-let ((d (and (slot-boundp self 'description) (cli-opt-description self))))
            (format stream ":  ~A" d)
            "")))

(defmethod cli-equal ((a cli-opt) (b cli-opt))
  (with-slots (name kind) a
    (with-slots ((bn name) (bk kind)) b
      (and (equal name bn)
           (equal kind bk)))))

(defmethod call-opt ((self cli-opt) arg)
  (funcall (cli-opt-thunk self) arg))

(defmethod do-opt ((self cli-opt))
  (setf (cli-opt-val self) (call-opt self (cli-opt-val self))))

(defmethod do-opts ((self vector))
  (loop for opt across self
        do (do-opt opt)))

(defmethod find-opt ((name string) (self list) &optional active)
  (let ((found (find name self :key 'cli-opt-name :test 'equal)))
    (if active
        (when (cli-lock-p found)
          found)
        found)))

(defmethod find-opt ((name string) (self vector) &optional active)
  (let ((found (find name self :key 'cli-opt-name :test 'equal)))
    (if active
        (when (cli-lock-p found)
          found)
        found)))

(defun getopt (name &optional (opts *opts*))
  "Retrieve a CLI-OPT-VAL by name from a vector of CLI-OPTs."
  (cli-opt-val (find-opt (string-downcase name) opts)))

(defun setopt (name val &optional (opts *opts*))
    (setf (cli-opt-val (find-opt (string-downcase name) opts)) val))

(defsetf getopt setopt)
