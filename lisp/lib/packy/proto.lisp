(in-package :packy/core)

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

(defclass abstract-package () ())

(defgeneric pack (self &key &allow-other-keys))
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
