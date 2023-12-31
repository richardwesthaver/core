(in-package :rdb)

;;; rdb-opts
(defvar *rdb-opts-lookup-table*
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (x) (setf (gethash (car x) table) (cdr x)))
          (loop for y across *rocksdb-options*
                collect (cons y (format nil "rocksdb-options-set-~x" y))))
    table))

(defmacro rdb-opt-setter (key)
  `(symbolicate (format nil "rocksdb-options-set-~x" ,key)))

(defun %set-rocksdb-option (opt key val)
  (funcall (rdb-opt-setter key) opt val))

;; (funcall (rdb-opt-setter "create-if-missing") (rocksdb-options-create) nil)

(defclass rdb-opts ()
  ((table :initarg :table :type hash-table :accessor rdb-opts-table)
   (sap :initarg :sap :type (or null alien) :accessor rdb-opts-sap)))

(defmethod initialize-instance ((self rdb-opts) &rest initargs &key &allow-other-keys)
  (with-slots (sap table) self
    (unless (getf initargs :table) (setf table (make-hash-table :test #'equal)))
    (unless (getf initargs :sap) (setf sap (rocksdb-options-create)))
    (loop for (k v) on initargs by #'cddr while v
          do (let ((k (typecase k
                        (string (string-downcase k))
                        (symbol (string-downcase (symbol-name k)))
                        (t (string-downcase (format nil "~s" k))))))
               (set-opt self k v)))
    self))

(defun make-rdb-opts (&rest values)
  (apply #'make-instance 'rdb-opts values))

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
  (%set-rocksdb-option (rdb-opts-sap self) key (get-opt self key)))
  
(defmethod push-sap* ((self rdb-opts))
  "Initialized the SAP slot with values from TABLE."
  (with-slots (table) self
    (loop for k across (hash-table-keys table)
          do (push-sap self k))))

(defun default-rdb-opts () 
  (make-rdb-opts :create-if-missing t))

;;; bytes
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

(defun make-rdb-val (val)
  "Convert VAL to an object of type RDB-VAL."
  (make-instance 'rdb-val :buffer val))

(defclass rdb-key (rdb-bytes)
  ()
  (:documentation "RDB key protocol.

Keys must be able to be encoded to and from (array unsigned-byte)."))

(defun make-rdb-key (key)
  "Convert KEY to an object of type RDB-KEY."
  (make-instance 'rdb-key :buffer key))

(defclass rdb-kv (rdb-bytes)
  ((key :initarg :key :type rdb-key)
   (val :initarg :val :type rdb-val)))

(defun make-rdb-kv (key val)
  "Generate a new RDB-KV pair."
  (make-instance 'rdb-kv 
    :key (make-rdb-key key) 
    :val (make-rdb-val val)))

;;; rdb-cf
(defstruct rdb-cf
  "RDB Column Family structure. Contains a name, a cons of (rdb-key-type
. rdb-val-type), and a system-area-pointer to the underlying
rocksdb_cf_t handle."
  (name "" :type string)
  (kv (make-instance 'rdb-kv) :type rdb-kv)
  (sap nil :type (or null alien)))

;; TODO: fix
(defun create-cf (db cf)
  (setf (rdb-cf-sap cf)
        (create-cf-raw db (rdb-cf-name cf))))

;;; rdb
(defstruct rdb
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (cfs (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0) :type (array rdb-cf))
  (db nil :type (or null alien)))

(defun create-db (name &key opts cfs)
  "Construct a new RDB instance from NAME and optional OPTS and DB-PTR."
  (make-rdb :name name 
            :opts (or opts (default-rdb-opts))
            :cfs (or cfs (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0))
            :db (open-db-raw name (if opts (rdb-opts-sap opts) (default-rocksdb-options)))))

(defmethod push-cf ((cf rdb-cf) (db rdb))
  (vector-push cf (rdb-cfs db)))

(defmethod open-db ((self rdb))
  (with-slots (db) self
    (unless (null db) (close-db self))
    (setf db (open-db-raw (rdb-name self) (rdb-opts-sap (rdb-opts self))))))

(defmethod close-db ((self rdb))  
  (with-slots (db) self
    (unless (null db)
      (close-db-raw db)
      (setf db nil))))

(defmethod destroy-db ((self rdb))  
  (when (rdb-db self) (close-db self))
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
