(defpackage :xdb
  (:use :cl :std :seq :db)
  (:export :xdb :dbs))

(in-package :xdb)

;;; XDB
(defclass xdb ()
  ((location :initarg :location
             :accessor location
             :initform (error "Location is required"))
   (collections :initarg :collections
                :accessor collections
                :initform (make-hash-table :test 'equal))))

(defclass dbs ()
  ((databases :initarg :databases
        :accessor databases
        :initform (make-hash-table :test 'equal))
   (base-path :initarg :base-path
              :initform "/tmp/db/"
              :accessor base-path)))

(defmethod get-db ((dbs dbs) name)
  (gethash name (databases dbs)))

(defun parse-db-path (path)
  (make-pathname :directory
                 (list* :relative
                        (etypecase path
                          (cons path
                           path)
                          (string path
                           (list path))))))

(defmethod add-db ((dbs dbs) name &key base-path load-from-file-p)
  (unless (gethash name (databases dbs))
    (let* ((base-path (or base-path (base-path dbs)))
           (db-path (merge-pathnames (parse-db-path name) base-path))
           (db (make-instance 'xdb :location db-path)))
      (ensure-directories-exist db-path)
      (setf (gethash name (databases dbs)) db)
      (if load-from-file-p
          (load-db db :load-from-file-p load-from-file-p)))))

(defparameter *dbs* nil)

(defun dbs ()
  *dbs*)

(defmethod initialize-doc-container ((collection collection))
  (setf (docs collection) (make-array 0 :adjustable t :fill-pointer 0)))

(defmethod map-docs (result-type function (collection collection)
                     &rest more-collections)
  (let ((result
          (map result-type function (docs collection))))
    (loop for collection in more-collections
          for results = (map result-type function (docs collection))
          if result-type
          do (setf result (concatenate result-type result results)))
    result))

(defmethod find-duplicate-doc ((collection collection) doc &key function)
  (let ((test (or function #'duplicate-doc-p)))
    (map-docs
     nil
     (lambda (docx)
       (when (funcall test doc docx)
         (return-from find-duplicate-doc docx)))
     collection)))

(defmethod add-doc ((collection collection) doc &key duplicate-doc-p-func)
  (when doc
    (if duplicate-doc-p-func
        (let ((dup (find-duplicate-doc collection doc :function duplicate-doc-p-func)))
          (if (not dup)
              (vector-push-extend doc (docs collection))
              (setf dup doc) ;;doing this because
              ))
        (vector-push-extend doc (docs collection)))))

(defmethod store-doc ((collection collection) doc
                      &key (duplicate-doc-p-func #'duplicate-doc-p))
  (let ((dup (and duplicate-doc-p-func
                  (find-duplicate-doc collection doc
                                      :function duplicate-doc-p-func))))
    ;; a document might be considered duplicate based on the data 
    ;;contained and not its eql status as lisp object so we have to replace
    ;;it in the array with the new object effectively updating the data.
    (if dup
        (setf dup doc)
        (vector-push-extend doc (docs collection)))
    (serialize-doc collection doc))
  collection)

(defmethod serialize-doc ((collection collection) doc &key)
  (let ((path (make-pathname :type "log" :defaults (db::path collection))))
    (ensure-directories-exist path)
    (db::save-doc collection doc path))
  doc)

(defmethod serialize-docs (collection &key duplicate-doc-p-func)
  (map-docs
   nil
   (lambda (doc)
     (store-doc collection doc
                :duplicate-doc-p-func duplicate-doc-p-func))
   collection))

(defmethod load-from-file ((collection collection) file)
  (when (probe-file file)
    (db::load-data collection file
               (lambda (object)
                 (add-doc collection object)))))

(defmethod get-collection ((db xdb) name)
  (gethash name (collections db)))

(defun make-new-collection (name db &key collection-class)
  (let ((collection
         (make-instance collection-class
                         :name name
                         :path (merge-pathnames name (location db)))))
    (initialize-doc-container collection)
    collection))

(defmethod add-collection ((db xdb) name
                           &key (collection-class 'collection) load-from-file-p)
  (let ((collection (or (gethash name (collections db))
                        (setf (gethash name (collections db))
                              (make-new-collection name db
                                                   :collection-class collection-class)))))
    (ensure-directories-exist (db::path collection))
    (when load-from-file-p
      (load-from-file collection
                      (make-pathname :defaults (db::path collection)
                                     :type "snap"))
      (load-from-file collection
                      (make-pathname :defaults (db::path collection)
                                     :type "log")))
    collection))

(defun file-date ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr)
                       (get-decoded-time)
    (format nil "~A~A~A_~A~A~A" yr mon day hr min sec)))

(defun append-date (name)
  (format nil "~a-~a" name (file-date)))

(defmethod snapshot ((collection collection))
  (let* ((backup (merge-pathnames "backup/" (db::path collection)))
         (log (make-pathname :type "log" :defaults (db::path collection)))
         (snap (make-pathname :type "snap" :defaults (db::path collection)))
         (backup-name (append-date (db::name collection)))
         (log-backup (make-pathname :name backup-name
                                    :type "log"
                                    :defaults backup))
         (snap-backup (make-pathname :name backup-name
                                     :type "snap"
                                     :defaults backup)))
    (ensure-directories-exist backup)
    (when (probe-file snap)
      (rename-file snap snap-backup))
    (when (probe-file log)
      (rename-file log log-backup))
    (db::save-data collection snap)))

(defmethod snapshot ((db xdb))
  (maphash (lambda (key value)
             (declare (ignore key))
             (snapshot value))
           (collections db)))

(defmethod load-db ((db xdb) &key load-from-file-p)
  (let ((unique-collections (make-hash-table :test 'equal)))
    (dolist (path (directory (format nil "~A/*.*" (location db))))
      (when (pathname-name path)
        (setf (gethash (pathname-name path) unique-collections)
              (pathname-name path))))
    (maphash  #'(lambda (key value)
                  (declare (ignore key))
                  (add-collection db value :load-from-file-p load-from-file-p))
              unique-collections)))

(defmethod get-docs ((db xdb) collection-name &key return-type)
  (let ((col (gethash collection-name (collections db))))
    (if return-type
        (coerce return-type
                (docs col))
        (docs col))))

(defmethod get-doc (collection value  &key (element 'key) (test #'equal))
  (map-docs
         nil
         (lambda (doc)
           (when (funcall test (get-val doc element) value)
             (return-from get-doc doc)))
         collection))

(defmethod get-doc-complex (test element value collection &rest more-collections)
  (apply #'map-docs
         nil
         (lambda (doc)
           (when (apply test (list (get-val doc element) value))
             (return-from get-doc-complex doc)))
         collection
         more-collections))

(defmethod find-doc (collection &key test)
  (if test
      (map-docs
       nil
       (lambda (doc)
         (when (funcall test doc)
           (return-from find-doc doc)))
       collection)))

(defmethod find-doc-complex (test collection &rest more-collections)
  (apply #'map-docs
         (lambda (doc)
           (when (funcall test doc)
             (return-from find-doc-complex doc)))
         collection
         (cdr more-collections)))

(defmethod find-docs (return-type test collection)
  (coerce (loop for doc across (docs collection)
                when (funcall test doc)
                collect doc)
          return-type))

(defclass union-docs ()
  ((docs :initarg :docs
         :accessor :docs)))

(defmethod union-collection (return-type (collection collection) &rest more-collections)
  (make-instance
   'union-docs
   :docs (apply #'map-docs (list return-type collection more-collections))))

(defclass join-docs ()
  ((docs :initarg :docs
          :accessor :docs)))

(defclass join-result ()
  ((docs :initarg :docs
          :accessor :docs)))

(defun sort-key (doc)
  (get-val doc 'key))

;;TODO: How to update log if collection is sorted? Make a snapshot?

(defmethod sort-collection ((collection collection)
                            &key return-sort
                            (sort-value-func #'sort-key) (sort-test-func  #'>))
  (setf (docs collection)
        (sort (docs collection)
              sort-test-func
              :key sort-value-func))
  (if return-sort
      (docs collection)
      t))

(defmethod db::sort-collection-temporary ((collection collection)
                            &key (sort-value-func #'sort-key) (sort-test-func  #'>))
  (let ((sorted-array (copy-array (docs collection))))
   (setf sorted-array
         (sort sorted-array
               sort-test-func
               :key sort-value-func))
   sorted-array))

(defun sort-docs (docs &key (sort-value-func #'sort-key) (sort-test-func  #'>))
  :documentation "Sorts array/list of docs and returns the sorted array."
  (sort docs
        sort-test-func
        :key sort-value-func))

;;Add method for validation when updating a collection.

(defclass xdb-sequence ()
  ((key :initarg :key
         :accessor key)
   (value :initarg :value
          :accessor value)))

(defmethod enable-sequences ((xdb xdb))
  (add-collection xdb "sequences" 
                :collection-class 'collection
                :load-from-file-p t))

(defmethod next-sequence ((xdb xdb) key)
  (let ((doc (get-doc (get-collection xdb "sequences") key)))
    (unless doc
      (setf doc (make-instance 'xdb-sequence :key key :value 0)))
    (incf (get-val doc 'value))
    (store-doc (get-collection xdb "sequences")
                doc)
    (get-val doc 'value)))
