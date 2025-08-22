;;; krypt/krypt.lisp --- Krypt API

;;

;;; Code:
(in-package :skel/krypt)

;;; Vars
(defparameter *kryptrc* (merge-pathnames ".kryptrc" (user-homedir-pathname)))
(defvar *krypt-directory* (merge-pathnames ".stash/krypt/" (user-homedir-pathname)))
(defvar *krypt-key-directory* (merge-pathnames "key/" *krypt-directory*))
(defvar *krypt-token-directory* (merge-pathnames "token/" *krypt-directory*))
(defvar *krypt-password-directory* (merge-pathnames "pw/" *krypt-directory*))
(defvar *krypt-net-directory* (merge-pathnames "net/" *krypt-directory*))
(defvar *krypt-user-config* nil)

;;; Config
(defconfig krypt-config (ast id)
  ((path :initform nil :initarg :path :type (or pathname null))
   (keyrings :initform nil :initarg :keyrings)
   (passwords :initform *krypt-password-directory* :initarg :passwords)
   (tokens :initform *krypt-token-directory* :initarg :tokens)
   (keys :initform *krypt-key-directory* :initarg :keys)
   (ssh :initform *user-ssh-directory* :initarg :ssh)
   (gpg :initform *user-gpg-directory* :initarg :gpg)))

(defmethod print-object ((self krypt-config) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (format-sxhash (id self)))))

(defun find-krypt-symbol (s)
  (find-symbol* (symbol-name s) :krypt nil))

(defmethod load-ast ((self krypt-config))
  (with-slots (ast) self
    (if (formp ast)
        ;; ast is valid, modify object, set ast nil
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-krypt-symbol k)))
              (setf (slot-value self s) v))) ;; needs to be correct package
          (setf (ast self) nil)
          (with-slots (passwords tokens keys) self
            (when (stringp passwords)
              (setf (slot-value self 'passwords) 
                    (pathname (ensure-directories-exist passwords))))
            (when (stringp tokens) 
              (setf (slot-value self 'tokens) 
                    (pathname (ensure-directories-exist tokens))))
            (when (stringp keys) 
              (setf (slot-value self 'keys) 
                    (pathname (ensure-directories-exist keys)))))
          self)
        ;; invalid ast, signal error
        (error 'syntax-error))))

(defmethod build-ast ((self krypt-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
         (unwrap-object self
                        :slots t
                        :methods nil
                        :nullp nullp
                        :exclude exclude)))

(defun load-kryptrc (&optional (file *kryptrc*))
  "Load a krypt configuration from FILE. Defaults to ~/.kryptrc."
  (unless (not (probe-file file))
    (let ((form (file-read-forms file)))
      (load-ast (make-instance 'krypt-config :ast form :path file :id (sxhash form))))))

(defun init-krypt ()
  "Initialize the global KRYPT environment:

*KRYPT-USER-CONFIG*"
  (mapc 'ensure-directories-exist 
        (list *krypt-directory* *krypt-net-directory*
              *krypt-token-directory* *krypt-password-directory*))
  (setq *krypt-user-config* (load-kryptrc))
  (values))
