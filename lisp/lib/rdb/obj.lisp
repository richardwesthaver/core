(in-package :rdb)

;;; rdb-opts
(defvar *rdb-opts-lookup-table*
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (x) (setf (gethash (car x) table) (cdr x)))
          (loop for y across *rocksdb-options*
                collect (cons y (format nil "rocksdb-options-set-~x" y))))
    table))

(defun rdb-opt-setter (key)
  (find-symbol (format nil "rocksdb-options-set-~x" key) :rocksdb))

(defun %set-rocksdb-option (opt key val)
  (funcall (rdb-opt-setter key) opt val))

;; (funcall (rdb-opt-setter "create-if-missing") (rocksdb-options-create) nil)

(defclass rdb-opts ()
  ((table :initform (make-hash-table :test #'equal) :type hash-table :accessor rdb-opts-table)
   (sap :type (or null alien) :accessor rdb-opts-sap)))

(defmethod initialize-instance :after ((self rdb-opts) &rest initargs &key &allow-other-keys)
  (with-slots (sap) self
    (unless (getf initargs :sap) (setf sap (rocksdb-options-create)))
    (loop for (k v) on initargs by #'cddr while v
          do (let ((k (typecase k
                        (string (string-downcase k))
                        (symbol (string-downcase (symbol-name k)))
                        (t (string-downcase (format nil "~x" k))))))
               (set-opt self k v)))
    self))

(defmethod get-opt ((self rdb-opts) key)
  "Return the current value of KEY in SELF if found, else return nil."
  (gethash key (rdb-opts-table self)))

(defmethod set-opt ((self rdb-opts) key val &key push)
  "Set the VAL of KEY in SELF with '(setf (gethash SELF KEY) VAL)'."
  (prog1
      (setf (gethash key (rdb-opts-table self)) val)
    (when push (push-sap self key))))

(defmethod push-sap ((self rdb-opts) key)
  "Push KEY from slot :TABLE to the instance :SAP."
  (%set-rocksdb-option (rdb-opts-sap self)  key (get-opt self key)))
  
(defmethod push-sap* ((self rdb-opts))
  "Initialized the SAP slot with values from TABLE."
  (with-slots (table) self
    (loop for k across (hash-table-keys table)
          do (push-sap self k))))

(declaim (inline default-rdb-opts))
(defun default-rdb-opts () 
  (make-instance 'rdb-opts
    :create-if-missing t 
    :total-threads 4
    :max-open-files 10000))

;;; rdb-cf

;; NOTE: read-sequence and write-sequence now accept user-extended
;; sequences

(defclass rdb-bytes (sequence)
    ((buffer :initarg :buffer :type (array unsigned-byte) :accessor rdb-bytes-buffer))
  (:documentation "RDB unsigned-byte array. Implements the iterator protocol."))

(defmethod sequence:length ((self rdb-bytes))
  (length (rdb-bytes-buffer self)))

(defmethod sequence:elt ((self rdb-bytes) index)
  (elt (rdb-bytes-buffer self) index))

(defmethod sequence:make-sequence-like ((self rdb-bytes) length &key initial-element initial-contents)
  (let ((res (make-instance 'rdb-bytes)))
    (cond 
      ((and initial-element initial-contents) (error "supplied both ~S and ~S to ~S" :initial-element :initial-contents 'make-sequence-like))
      (initial-element (setf (rdb-bytes-buffer res) (make-array length :element-type (array-element-type self)
                                                                       :initial-element initial-element)))
      (initial-contents (setf (rdb-bytes-buffer res) (make-array length :element-type (array-element-type self)
                                                                       :initial-contents initial-contents)))
      (t (setf (rdb-bytes-buffer res) (make-array length :element-type (array-element-type self)))))))

;; (sequence:make-sequence-iterator (make-instance 'rdb-bytes :buffer (vector 1 2 3)))
(defmethod sequence:make-sequence-iterator ((self rdb-bytes) &key from-end start end)
  (sequence:make-sequence-iterator (rdb-bytes-buffer self) :from-end from-end :start start :end end))

;; (defmethod sequence:subseq ((self rdb-bytes) start &optional end))
;; (defmethod sequence:concatenate ((self rdb-bytes) &rest sequences))

(defclass rdb-val (rdb-bytes)
  ()
  (:documentation "RDB value protocol.

Values must be able to be encoded to and from (array unsigned-byte)."))

(defclass rdb-key (rdb-bytes)
  ()
  (:documentation "RDB key protocol.

Keys must be able to be encoded to and from (array unsigned-byte)."))

(defclass rdb-kv (rdb-bytes)
  ((key :initarg :key :type rdb-key)
   (val :initarg :val :type rdb-val)))

(defun make-rdb-key (key)
  "Convert KEY to an object of type RDB-KEY."
  (make-instance 'rdb-key :buffer key))

(defstruct rdb-cf
  "RDB Column Family structure. Contains a name, a cons of (rdb-key-type
. rdb-val-type), and a system-area-pointer to the underlying
rocksdb_cf_t handle."
  (name "" :type string)
  (kv (make-instance 'rdb-kv) :type rdb-kv)
  (sap nil :type (or null alien)))

(defun create-cf (db cf)
  (setf (rdb-cf-sap cf)
        (with-errptr err
          (rocksdb-create-column-family db (rocksdb-options-create) (rdb-cf-name cf) err))))

;;; rdb
(defstruct rdb
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (db nil :type (or null alien))
  (cfs (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0) :type (array rdb-cf)))

(defmethod push-cf ((cf rdb-cf) (db rdb))
  (vector-push cf (rdb-cfs db)))

(defmethod open-db ((self rdb))
  (setf (rdb-db self)
        (open-db-raw (rdb-name self) (rdb-opts-sap (rdb-opts self)))))

(defmethod close-db ((self rdb))  
  (close-db-raw (rdb-db self))
  (setf (rdb-db self) nil))

(defmethod destroy-db ((self rdb))  
  (when (rdb-db self) (close-rdb self))
  (destroy-db-raw (rdb-name self)))

(defmethod init-db ((self rdb))
  (loop for cf across (rdb-cfs self)
        do (create-cf (rdb-db self) cf)))

(defmethod insert-key ((self rdb) key val &key cf)
  (if cf
    (put-cf-raw
     (rdb-db self)
     (rdb-cf-sap (find cf (rdb-cfs self) :key #'rdb-cf-name :test #'equal))
     key
     val)
    (put-kv-raw
     (rdb-db self)
     key 
     val)))
