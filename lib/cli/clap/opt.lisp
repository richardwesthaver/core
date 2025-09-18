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
  (type 'boolean :type (or symbol list))
  (thunk 'default-opt-thunk :type symbol)
  (val nil)
  (description nil :type (or null string))
  (lock nil :type boolean))

(defaccessor cli-thunk ((self cli-opt)) (cli-opt-thunk self))
(defaccessor cli-name ((self cli-opt)) (cli-opt-name self))

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
   :slot-names '(name type thunk val description lock)
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
  (format stream "-~(~{~A~^/--~}~)~@[ :value ~A~]~24t~@[~A~]~@[~%~4t:doc ~A~]"
          (let ((n (cli-opt-name self)))
            (declare (simple-string n))
            (list (make-shorty n) n))
          (and (slot-boundp self 'val) (cli-opt-val self))
          (and (slot-boundp self 'description) (cli-opt-description self))
          (when (fboundp (cli-thunk self))
            (documentation (symbol-function (cli-thunk self)) 'function))))

(defmethod equiv ((a cli-opt) (b cli-opt))
  (with-slots (name type) a
    (with-slots ((bn name) (bk type)) b
      (and (equal name bn)
           (equal type bk)))))

(defmethod equiv ((a t) (b cli-opt))
  (equalp (cli-opt-val b) a))

(defmethod equiv ((a cli-opt) (b t))
  (equalp (cli-opt-val a) b))

(defmethod call-opt ((self cli-opt) arg)
  (funcall (cli-opt-thunk self) arg))

(defmethod do-opt ((self cli-opt))
  (prog1 (setf (cli-opt-val self) (call-opt self (cli-opt-val self)))
    (setf (cli-opt-lock self) nil)))

(defmethod do-opts ((self vector))
  (loop for opt across self
        do (do-opt opt)))

(defmethods find-opt 
  (((name string) (self list) &key active default)
   (if-let ((found (find name self :key 'cli-opt-name :test 'equal)))
     (if active
         (when (cli-lock-p found)
           found)
         found)
     default))
  (((name string) (self vector) &key active default)
   (if-let ((found (find name self :key 'cli-opt-name :test 'equal)))
     (if active
         (when (cli-lock-p found)
           found)
         found)
     default)))

(defun getopt (name &optional (default :error) (opts *opts*))
  "Retrieve a CLI-OPT-VAL by name from a vector of CLI-OPTs."
  (let ((opts (or opts (opts *cli*))))
    (cli-opt-val (find-opt 
                  (string-downcase name) opts 
                  :default (if (eql default :error)
                               (clap-unknown-argument name 'opt)
                               default)))))

(defun setopt (name val &optional (default :error) (opts *opts*))
  (let ((opts (or opts (opts *cli*))))
    (setf (cli-opt-val 
           (find-opt 
            (string-downcase name) opts 
            :default (if (eql default :error)
                         (clap-unknown-argument name 'opt)
                         default)))
          val)))

(defsetf getopt setopt)

(defmacro with-opt-restart-case (arg expression)
  "Bind restarts 'use-as-arg' and 'discard-arg' for duration of EXPRESSION."
  `(restart-case ,expression
     (use-as-arg () () (make-cli-node 'arg ,arg))
     (discard-arg () () (setf ,arg nil))))
