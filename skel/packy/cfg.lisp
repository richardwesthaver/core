;;; cfg.lisp --- Packy Configuration

;; 

;;; Code:
(in-package :skel/packy)

(defconfig target-config (ast id)
  ((name :accessor name)))

(defconfig packy-config (ast id)
  ((path :initarg :path :accessor path)
   (checksum :initarg :checksum :initform :sha256)
   (man-dirs :initarg :man-dirs)
   (doc-dirs :initarg :doc-dirs)
   (purge-targets :initarg :purge-targets)
   (pkgext :initarg :pkgext)
   (srcext :initarg :srcext)
   (compression :initarg :compression)
   (options :initarg :options)
   (builddir :initarg :builddir)
   (buildenv :initarg :buildenv)
   (targets :initarg :target :type (vector target-config))
   (langs :initarg :langs :type (vector lang-config))))

(defmethod make-config ((self (eql :packy)) &key ast path)
  (make-instance 'packy-user-config :ast ast :path path))

(defmethod load-ast ((self packy-config))
  (with-slots (ast) self
    (if (formp ast)
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-symbol (symbol-name k) :skel/packy))) ;; needs to be correct package
              (setf (slot-value self s) v)))
          (unless *keep-ast* (setf (ast self) nil))
          self)
        ;; invalid ast, signal error
        (error 'syntax-error))))

(defmethod load-config ((self (eql :packy)) (from pathname) &key)
  (let ((c (make-config :packy)))
    (load-ast c)))
