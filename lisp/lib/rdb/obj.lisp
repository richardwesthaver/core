(in-package :rdb)

;;; rdb-opts
(defvar *rdb-opts-lookup-table*
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (x) (setf (gethash (car x) table) (cdr x)))
          (loop for y across *rocksdb-options*
                collect (cons y (format nil "~:@(rocksdb-options-set-~x~)" y))))
    table))

(defmacro rdb-opt-setter (key)
  `(find-symbol (format nil "~:@(rocksdb-options-set-~x~)" ,key) :rocksdb))

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
  (let ((opts (apply #'make-instance 'rdb-opts values)))
    (push-sap* opts)
    opts))

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
    (loop for k in (hash-table-keys table)
          do (push-sap self k))))

(defun default-rdb-opts () 
  ;; TODO 2024-03-10: handle lisp->C types
  (make-rdb-opts :create-if-missing 1))

;;; bytes
(defclass rdb-bytes ()
    ((buffer :initarg :buffer :type octet-vector :accessor rdb-bytes-buffer))
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

;;; keyval
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
  ((key :initarg :key :type rdb-key :accessor rdb-key)
   (val :initarg :val :type rdb-val :accessor rdb-val)))

(defun make-rdb-kv (key val)
  "Generate a new RDB-KV pair."
  (make-instance 'rdb-kv 
    :key (make-rdb-key key) 
    :val (make-rdb-val val)))

(defvar *default-rdb-kv* (make-rdb-kv #() #()))

;;; column family
(defstruct (rdb-cf (:constructor make-rdb-cf (name &key kv sap)))
  "RDB Column Family structure. Contains a name, a cons of (rdb-key-type
. rdb-val-type), and a system-area-pointer to the underlying
rocksdb_cf_t handle."
  (name "" :type string)
  (kv *default-rdb-kv* :type rdb-kv)
  (sap nil :type (or null alien)))
  
;;; rdb
(defstruct (rdb (:constructor make-rdb (name opts cfs &optional db)))
  (name "" :type string)
  (opts (default-rdb-opts) :type rdb-opts)
  (cfs (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0) :type (array rdb-cf))
  (db nil :type (or null alien)))

;; (defvar *default-rdb-opts* (default-rdb-opts))

(defun create-db (name &key opts cfs open)
  "Construct a new RDB instance from NAME.

OPTS = rdb-opts
CFS = (sequence rdb-cf)
OPEN = boolean

When OPEN is non-nil, the database and all column families are opened
and internal sap slots are initialized."
  (when (probe-file name) (log:warn! "directory already exists: " name))
  (let* ((opts (or opts (default-rdb-opts)))
         (obj
          (make-rdb (typecase name
                      (pathname (namestring name))
                      (string name)
                      (t (error "invalid NAME: ~S" name)))
                    opts
                    (or (when cfs
                          (typecase cfs
                            (list (coerce cfs 'vector))
                            (vector cfs)
                            (rdb-cf (vector cfs))
                            (t (log:warn! "invalid CF passed to create-db"))))
                        (make-array 0 :element-type 'rdb-cf :adjustable t :fill-pointer 0)))))
    (when open
      (open-db obj)
      (create-cfs obj))
    obj))

(defmethod push-cf ((cf rdb-cf) (db rdb))
  (vector-push cf (rdb-cfs db)))

;; TODO: fix
(defmethod create-cf ((db rdb) (cf rdb-cf))
  (setf (rdb-cf-sap cf)
        (create-cf-raw (rdb-db db) (rdb-cf-name cf))))

(defmethod close-cf ((cf rdb-cf))
  (with-slots (sap) cf
    (free-alien sap)))

(defmethod open-db ((self rdb))
  (with-slots (name db opts) self
    (or
     db 
     (setf db (open-db-raw name (rdb-opts-sap opts))))))

(defmethod destroy-db ((self rdb))  
  (when (rdb-db self) (close-db self))
  (destroy-db-raw (rdb-name self)))

(defmethod create-cfs ((self rdb) &key &allow-other-keys)
  (loop for cf across (rdb-cfs self)
        do (create-cf self cf)))

(defmethod close-cfs ((self rdb) &key &allow-other-keys)
  (with-slots (cfs) self
    (declare (type (array rdb-cf) cfs))
    (loop for cf across cfs
          do (progn
               (close-cf cf)
               (free-alien (rdb-cf-sap cf))))))

(defmethod close-db ((self rdb) &key &allow-other-keys)
  (with-slots (db cfs) self
    (unless (null db)
      (close-cfs self)
      (close-db-raw db)
      (free-alien db))))


(defmethod put-key ((self rdb) key val)
  (put-kv-raw
   (rdb-db self)
   key 
   val))

(defmethod put-kv ((self rdb) (kv rdb-kv))
  (put-kv-raw
   (rdb-db self)
   (rdb-key kv)
   (rdb-val kv)))

(defmethod insert-key ((self rdb) key val &key cf)
  (if cf
    (put-cf-raw
     (rdb-db self)
     (rdb-cf-sap (find cf (rdb-cfs self) :key #'rdb-cf-name :test #'equal))
     key
     val)
    (put-key self key val)))

(defmethod insert-kv ((self rdb) (kv rdb-kv) &key cf)
  (if cf
      (put-cf-raw (rdb-db self)
                  (rdb-cf-sap
                   (find cf (rdb-cfs self)
                         :key #'rdb-cf-name
                         :test #'equal))
                  (rdb-bytes-buffer (rdb-key kv))
                  (rdb-bytes-buffer (rdb-val kv)))
      (put-kv self kv)))
