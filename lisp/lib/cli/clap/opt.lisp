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
  (thunk nil :type (or null function symbol))
  (val nil)
  (global nil :type boolean)
  (description nil :type (or null string))
  (lock nil :type boolean))

(defun %compose-short-opt (o arg)
  (declare (ignorable arg))
  (setf (cli-opt-val o) t)
  (make-cli-node 'opt o))

(defun %compose-long-opt (o args)
  (declare (ignorable args))
  (setf (cli-opt-val o) (or (pop args) t))
  (make-cli-node 'opt o))

(defmethod handle-unknown-argument ((self cli-opt) arg))
(defmethod handle-missing-argument ((self cli-opt) arg))
(defmethod handle-invalid-argument ((self cli-opt) arg))

(defmethod initialize-instance :after ((self cli-opt) &key)
  (with-slots (name thunk) self
    (unless (stringp name) (setf name (format nil "~(~A~)" name)))
    (when (symbolp thunk) (setf thunk (funcall (compile nil `(lambda () ,(symbol-function thunk))))))
    self))

(defmethod install-thunk ((self cli-opt) (lambda function) &optional compile)
  "Install THUNK into the corresponding slot in cli-cmd SELF."
  (let ((%thunk (if compile (compile nil lambda) lambda)))
    (setf (cli-thunk self) %thunk)
    self))

(defmethod print-object ((self cli-opt) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :global ~A :val ~A"
            (cli-opt-name self)
            (cli-opt-global self)
            (cli-opt-val self))))

(defmethod print-usage ((self cli-opt) &optional stream)
  (format stream "-~(~{~A~^/--~}~)~A~A"
          (let ((n (cli-opt-name self)))
            (declare (simple-string n))
            (list (make-shorty n) n))
          (if (cli-opt-global self) "* " " ")
          (if-let ((d (and (slot-boundp self 'description) (cli-opt-description self))))
            (format stream ":  ~A" d)
            "")))

(defmethod cli-equal ((a cli-opt) (b cli-opt))
  (with-slots (name global kind) a
    (with-slots ((bn name) (bg global) (bk kind)) b
      (and (equal name bn)
           (eq global bg)
           (equal kind bk)))))

(defmethod call-opt ((self cli-opt) arg)
  (when-let ((thunk (cli-opt-thunk self)))
    (setf (cli-opt-val self) (funcall thunk arg))))

(defmethod do-opt ((self cli-opt))
  (call-opt self (cli-opt-val self)))

(defmethod do-opts ((self vector) &optional global)
  (declare (ignore global))
  (loop for opt across self
        do (do-opt opt)))

(defun active-global-opt-p (opt)
  "Return non-nil if OPT is active at runtime and global."
  (and (cli-opt-lock opt) (cli-opt-global opt)))
