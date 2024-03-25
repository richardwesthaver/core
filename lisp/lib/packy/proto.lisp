(in-package :packy/core)

(defclass pk-id (id)
  ((id :initform (make-array 16 :element-type 'octet) :initarg :id :type (octet-vector 16) :accessor id)))

(defmethod make-id ((kind (eql :pk)))
  (make-instance 'pk-id))

(defmethod make-random-id ((kind (eql :pk)))
  (let ((l))
    (dotimes (i 16) (push (random 255) l))
    (let ((v (make-array 16 :element-type 'octet
                            :initial-contents l)))
      (make-instance 'pk-id :id v))))
    
(defmethod print-object ((self pk-id) stream)
  (print-unreadable-object (self stream)
    (format stream "~A" (octet-vector-to-hex-string (id self)))))

(defclass abstract-package (id) ())

(defgeneric pk-pack (self &key &allow-other-keys))
(defgeneric pk-unpack (self &key &allow-other-keys))
(defgeneric pk-install (self &key &allow-other-keys))
(defgeneric pk-uninstall (self &key &allow-other-keys))
(defgeneric pk-update (self &key &allow-other-keys))
(defgeneric pk-push (self &key &allow-other-keys))
(defgeneric pk-pull (self &key &allow-other-keys))
(defgeneric pk-query (self &key &allow-other-keys))
(defgeneric pk-sync (self &key &allow-other-keys))
(defgeneric pk-build (self &key &allow-other-keys))
