;;; skel/core/obj.lisp --- Skel Objects

;; SKEL classes and methods

;;; Code:
(in-package :skel/core)

(defclass skel (id)
  ()
  (:documentation "Base class for skeleton objects."))

(declaim (inline sk-slot-name sk-class-name))
(defun sk-class-name (self &optional downcase)
  (let* ((class-name (string (class-name (class-of self))))
         (match (search "SK-" class-name :test 'equal :start1 0 :end1 3))
         (ret (if match
                  (subseq class-name 3)
                  class-name)))
    (if downcase
        (string-downcase ret)
        ret)))

(defun sk-slot-name (self &optional downcase) 
  (keywordicate (sk-class-name self downcase)))

(defmethod sk-new ((self t) &rest initargs)
  (apply #'make-instance self initargs))

(defmethod print-object ((self skel) stream)
  (print-unreadable-object (self stream)
    (format stream "~A :ID ~A" (sk-class-name self t) (format-sxhash (id self)))))

(defmethod initialize-instance :around ((self skel) &rest initargs &key &allow-other-keys)
  ;; TODO 2023-09-10: make fast 
  (unless (getf initargs :id)
    (setf (id self) (sxhash self)))
  (when (next-method-p)
    (call-next-method)))

;; TODO 2023-09-11: research other hashing strategies - maybe use the
;; sxhash as a nonce for UUID
;; note that the sk-meta class does not inherit from skel or ast.
;;; Meta
(defclass sk-meta (project-metadata) ()
  (:documentation "Skel Meta class."))

(defmethod print-object ((self sk-meta) stream)
  (print-unreadable-object (self stream)
    (format stream "~A ~A :path ~A" (sk-class-name self t) (name self) (path self))
    (unless (sequence:emptyp (version self))
      (format stream " :version ~A" (version self)))
    (format stream " :id ~A" (format-sxhash (id self)))))

(defun sk-init (class &rest initargs)
  (apply #'make-instance class initargs))

(defmacro sk-init-dir (class &rest initargs)
  `(let ((self (sk-init ',class ,@initargs)))
     (unless (getf ',initargs :path)
       (setf (path self) (sb-posix:getcwd)))
     self))

(defmacro sk-init-file (class &rest initargs)
  `(let ((self (sk-init ',class ,@initargs)))
     (unless (getf ',initargs :path)
       (setf (path self) *default-skelfile*))
     self))

;;; Config
(defconfig sk-config (skel ast) 
  ((vc :initform *default-vc-kind* :initarg :vc :type (or vc-repo vc-designator) :accessor vc)
   (store :initform *skel-store* :initarg :store :type pathname :accessor store)
   (stash :initform *skel-stash* :initarg :stash :type pathname :accessor stash)
   (cache :initform *skel-cache* :initarg :cache :type pathname :accessor cache)
   (data :initform *skel-data* :initarg :data :type pathname :accessor data)
   (scripts :initform nil :initarg :scripts :type (or pathname list (vector pathname)) :accessor scripts)
   (license :initarg :license :accessor license)
   (logger :initform (default-logger-config) :initarg :logger :type (or null logger-config) :accessor logger)
   (fmt :initform :pretty :initarg :fmt :type symbol)
   (auto-insert :initform nil :initarg :auto-insert :type form))
  (:documentation "Root configuration class for the SKEL system. This class doesn't need to be exposed externally, but specifies all shared fields of SK-*-CONFIG types."))

(defmethod sk-new ((self (eql :config)) &rest args &key (type :user))
  (setf self
        (case type
          (:user 'sk-user-config)
          (:system 'sk-system-config)
          (t 'sk-config)))
  (apply #'sk-new self args))

(declaim (inline bound-string-p sk-dir))
(defun bound-string-p (o s) (and (slot-boundp o s) (stringp (slot-value o s))))
(defun sk-dir (o)
  (let ((str (directory-namestring (path o))))
    (if (sb-sequence:emptyp str)
        *default-pathname-defaults*
        (pathname str))))

(defmacro with-skel-ast (sym obj &body body)
  `(with-slots ((,sym ast)) ,obj
     (if (formp ,sym)
         (progn ,@body)
         (invalid-ast ast))))

(defmethod load-ast ((self sk-config))
  ;; internal ast is never tagged
  (with-skel-ast ast self
    (sb-int:doplist (k v) ast
      (when-let ((s (find-sk-symbol k)))
        (setf (slot-value self s) v))) ;; needs to be the correct package
    (when (bound-string-p self 'stash) (setf (stash self) (merge-pathnames (stash self) (sk-dir self))))
    (when (bound-string-p self 'store) (setf (store self) (merge-pathnames (store self) (sk-dir self))))
    (when (bound-string-p self 'cache) (setf (cache self) (merge-pathnames (cache self) (sk-dir self))))
    (when (bound-string-p self 'data) (setf (data self) (data self)))
    ;; SCRIPTS
    (if (bound-string-p self 'scripts)
        (if-let* ((path (probe-file (pathname (the simple-string (scripts self))))))
                 (setf (scripts self)
                       (if (directory-path-p path)
                           (find-files path)
                           (list path)))
                 (warn! (format nil "ignoring missing scripts directory: ~A" (scripts self)))))
    (unless *keep-ast* (setf (ast self) nil))
    self))

(defmethod build-ast ((self sk-config) &key (nullp nil) (exclude '(ast id author version user)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

(defmethod sk-write-file ((self sk-config) 
                          &key (path *default-skelfile*) 
                               nullp
                               comment
                               (pretty t)
                               (if-exists :error))
  (build-ast self :nullp nullp)
  (prog1 
      (with-open-file (out path
                           :direction :output
                           :if-exists if-exists
                           :if-does-not-exist :create)
        (when comment (princ
                      (make-source-header-comment
                       (name self)
                       :cchar #\;
                       :timestamp t
                       :description (description self)
                       :opts '("mode:skel;"))
                      out))
        (write-ast self out :pretty pretty))
    (unless *keep-ast* (setf (ast self) nil))))

(defmethod write-ast ((self sk-config) stream &key (pretty t) (case :downcase))
  (if pretty
      (if (listp (ast self))
          (with-open-stream (st stream)
            (loop for (k v . rest) on (ast self)
                  by #'cddr
                  unless (or (null v) (null k))
                  do 
                     (write k :stream stream :pretty pretty :case case :readably t :array t :escape t)
                     (write-char #\space st)
                     (if (or (eq (type-of v) 'skel) (subtypep (type-of v) 'structure-object))
                         (write-ast v stream :pretty pretty)
                         (write v :stream stream :pretty pretty :case case :readably t :array t :escape t))
                     (write-char #\newline st)))
          (invalid-ast (ast self)))
      (write (ast self) :stream stream :pretty pretty :case case :readably t :array t :escape t)))

(defclass sk-system-config (sk-config sk-meta) ())

(defun default-sk-system-config ()
  (make-instance 'sk-system-config))

(defclass sk-user-config (sk-config sk-meta)
  ((user :initarg :user :type string :accessor user :initform (current-user))
   (name :initarg :name :type string :accessor name)
   (email :initarg :email :type string :accessor email))
  (:documentation "User configuration object, typically written to ~/.skelrc."))

(defmethod make-config ((self (eql :skel)) &rest args)
  (apply 'make-instance 'sk-user-config args))

(defun default-sk-user-config () (make-instance 'sk-user-config))

(declaim (type (or sk-user-config null) *skel-user-config*))
(declaim (type (or sk-system-config null) *skel-system-config*))
(defvar *skel-user-config* nil)
(defvar *skel-system-config* nil)
