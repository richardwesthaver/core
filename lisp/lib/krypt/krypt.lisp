;;; krypt/krypt.lisp --- Krypt API

;;

;;; Code:
(in-package :krypt)

(defparameter *default-user-kryptrc* (merge-pathnames ".kryptrc" (user-homedir-pathname)))

(defclass krypt-config (ast id)
  ((path :initform nil :initarg :path :type (or pathname null))))

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
          self)
        ;; invalid ast, signal error
        (error 'sxp-syntax-error))))

(defmethod build-ast ((self krypt-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
         (unwrap-object self
                        :slots t
                        :methods nil
                        :nullp nullp
                        :exclude exclude)))

(defun load-kryptrc (&optional (file *default-user-kryptrc*))
  "Load a krypt configuration from FILE. Defaults to ~/.kryptrc."
  (unless (not (probe-file file))
    (let ((form (file-read-forms file)))
      (load-ast (make-instance 'krypt-config :ast form :path file :id (sxhash form))))))
