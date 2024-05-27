;;; obj/meta/storable.lisp --- Storable Objects

;; The storable-class can be assigned to the :metaclass option of a
;; class to allow persistent storage of an object on disk. The
;; storable-slot-mixin is a custom slot option which can be used to
;; selectively enable slot serialization.

;;; Commentary:

;; This code is derived from XDB.

;; Note that this is not a general purpose SerDe. It is specifically designed
;; to decode/encode objects as single octet-vectors from/to an open stream
;; with minimal overhead. There is a separate interface for general-purpose
;; data encoding which can be found in the DAT system.

;;; Code:
(in-package :obj/meta/storable)

(sb-ext:unlock-package :sb-pcl)

;;; MOP
(defclass storable-class (standard-class)
  ((class-id :initform nil
             :accessor class-id)
   (slots-to-store :initform nil :accessor slots-to-store)
   (slot-locations-and-initforms
    :initform nil
    :accessor slot-locations-and-initforms)
   (all-slot-locations-and-initforms
    :initform nil
    :accessor all-slot-locations-and-initforms)
   (initforms :initform #()
              :accessor class-initforms)
   (id-cache :initarg :id-cache
             :initform (make-hash-table :size 1000)
             :accessor id-cache)))


;;; Initialize
(defun initialize-storable-class (next-method class &rest args
                                  &key direct-superclasses &allow-other-keys)
  (apply next-method class
         (if direct-superclasses
             args
             (list* :direct-superclasses (list (find-class 'storable-class))
                    args))))

(defmethod initialize-instance :around ((class storable-class)
                                        &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

(defmethod reinitialize-instance :around ((class storable-class)
                                          &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

;;; Validate
(defmethod validate-superclass
    ((class standard-class)
     (superclass storable-class))
  t)

(defmethod validate-superclass
    ((class storable-class)
     (superclass standard-class))
  t)

;;; Slot mixin
(defclass storable-slot-mixin ()
  ((storep :initarg :storep
           :initform t
           :accessor store-slot-p)))

(defclass storable-direct-slot-definition (storable-slot-mixin
                                           standard-direct-slot-definition)
  ())

(defclass storable-effective-slot-definition
    (storable-slot-mixin standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class storable-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'storable-direct-slot-definition))

(defmethod effective-slot-definition-class ((class storable-class)
                                            &key &allow-other-keys)
  (find-class 'storable-effective-slot-definition))

(defmethod compute-effective-slot-definition
    ((class storable-class) slot-name direct-definitions)
  (declare (ignore slot-name))
  (let ((effective-definition (call-next-method))
        (direct-definition (car direct-definitions)))
    (setf (store-slot-p effective-definition)
          (store-slot-p direct-definition))
    effective-definition))

(defun make-slots-cache (slot-definitions)
  (map 'vector
       (lambda (slot-definition)
	 (cons (sb-mop:slot-definition-location slot-definition)
	       (sb-mop:slot-definition-initform slot-definition)))
       slot-definitions))

(defun initialize-class-slots (class slots)
  (let* ((slots-to-store (coerce (remove-if-not #'store-slot-p slots)
                                 'simple-vector)))
    (setf (slots-to-store class)
          slots-to-store)
    (setf (slot-locations-and-initforms class)
          (make-slots-cache slots-to-store))
    (setf (all-slot-locations-and-initforms class)
          (make-slots-cache slots))
    (setf (class-initforms class)
          (map 'vector #'sb-mop:slot-definition-initform slots))))

(defmethod compute-slots :around ((class storable-class))
  (let ((slots (call-next-method)))
    (initialize-class-slots class slots)
    slots))


(sb-ext:lock-package :sb-pcl)
