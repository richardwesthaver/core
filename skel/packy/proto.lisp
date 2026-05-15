;;; proto.lisp --- Packy Protocol

;; 

;;; Code:
(in-package :skel/packy)

(define-condition packy-condition () ())
(deferror packy-error (simple-error packy-condition) () (:reporter t))

(defclass package-id (id)
  ((id :initform (make-array 16 :element-type 'octet) :initarg :id :type (octet-vector 16) :accessor id)))

(defmethod make-id ((kind (eql :package)))
  (make-instance 'package-id))

(defmethod make-random-id ((kind (eql :package)))
  (let ((l))
    (dotimes (i 16) (push (random 255) l))
    (let ((v (make-array 16 :element-type 'octet
                            :initial-contents l)))
      (make-instance 'package-id :id v))))
    
(defmethod print-object ((self package-id) stream)
  (print-unreadable-object (self stream)
    (format stream "~A" (octet-vector-to-hex-string (id self)))))

(defclass package-stream (pack io-stream) ())

(defclass compressed-package (package-stream decompressing-stream) ())

(defclass file-package (package-stream file-stream) ())

(defclass directory-package (package-stream)
  ((directory :initarg :directory :accessor dir)))

(defun packed-path-p (path)
  "Return non-nil if PATH is a valid package (.pkg.tar.zst)."
  (when-let ((name (pathname-name path))
             (type (pathname-type path)))
    (and (probe-file path) 
         (string-equal "zst" type)
         (let ((len (length name)))
           (string-equal "pkg.tar" (subseq name (- len 7)))))))

(defgeneric pack (self &key &allow-other-keys)
  (:method ((self pathname) &key)
    (when (packed-path-p self)
      (packy-error "Already a finalized package: ~A" self))))

(defgeneric unpack (self &key &allow-other-keys))
(defgeneric install-package (self &key &allow-other-keys))
(defgeneric uninstall-package (self &key &allow-other-keys))
(defgeneric update-package (self &key &allow-other-keys))
(defgeneric push-package (self &key &allow-other-keys))
(defgeneric pull-package (self &key &allow-other-keys))
(defgeneric query-package (self &key &allow-other-keys))
(defgeneric sync-package (self &key &allow-other-keys))
(defgeneric build-package (self &key &allow-other-keys))
(defgeneric prepare-package (self &key &allow-other-keys))
(defgeneric check-package (self &key &allow-other-keys))
(defgeneric package-version (self &key &allow-other-keys))
