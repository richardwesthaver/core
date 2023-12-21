;;; lib/obj/db/proto.lisp --- Database Protocol

;;

;;; Code:
(in-package :obj/db)

;;; Common
(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))

(defgeneric get-val (object element &key data-type)
  (:documentation "Returns the value in a object based on the supplied element name and possible type hints."))

(defgeneric (setf get-val) (new-value object element &key data-type)
  (:documentation "Set the value in a object based on the supplied element name and possible type hints."))

(defmethod get-val (object element &key data-type)
  (when object
    (typecase (or data-type object)
      (hash-table
       (gethash element object))
      (standard-object
       (slot-val object element))
      (t
       (if data-type
           (cond 
             ((equal 'alist data-type)
              (second (assoc element object :test #'equal)))
             ((equal 'plist data-type)
              (get object element))
             (t
              (error "Does not handle this type of object. Implement your own get-val method.")))
           (if (listp object)
               (second (assoc element object :test #'equal))
               (error "Does not handle this type of object. Implement your own get-val method.")))))))

(defmethod (setf get-val) (new-value object element &key data-type)
  (typecase (or data-type object)
    (hash-table (setf (gethash element object) new-value))
    (standard-object (setf (slot-value object element) new-value))
    (t
     (if data-type
         (cond ((equal 'alist data-type)
                (replace object (list (list element new-value))))
               ((equal 'plist data-type)
                ;;TODO: Implement this properly.
                (get object element ))
               (t
              (error "Does not handle this type of object. Implement your own get-val method.")))
         (if (listp object)
              (replace object (list (list element new-value)))
              (error "Does not handle this type of object. Implement your own get-val method."))))))

;;; DB
(defgeneric get-db (dbs name)
    (:documentation "Returns the xdb by name."))

(defgeneric add-db (dbs name &key base-path load-from-file-p)
  (:documentation "Adds a xdb to the dbs hashtable. A base-path can be
supplied here that is independatn of the dbs base-path so that a
database collection can be build that spans multiple disks etc."))

(defgeneric initialize-doc-container (collection)
  (:documentation
   "Create the docs container and set the collection's docs to the container.
If you specialize this then you have to specialize add-doc, store-doc,
sort-collection, sort-collection-temporary and union-collection. "))

(defgeneric map-docs (result-type function collection &rest more-collections)
  (:documentation
   "Applies the function accross all the documents in the collection"))

(defgeneric duplicate-doc-p (doc test-doc)
  (:method ((a t) (b t))))

(defgeneric find-duplicate-doc (collection doc &key function)
  (:documentation "Load collection from a file."))

(defgeneric add-doc (collection doc &key duplicate-doc-p-func)
  (:documentation "Add a document to the docs container."))

(defgeneric store-doc (collection doc &key duplicate-doc-p-func)
  (:documentation "Serialize the doc to file and add it to the collection."))

(defgeneric serialize-doc (collection doc &key)
  (:documentation "Serialize the doc to file."))

(defgeneric serialize-docs (collection &key duplicate-doc-p-func)
  (:documentation "Store all the docs in the collection on file and add it to the collection."))

(defgeneric load-from-file (collection file)
  (:documentation "Load collection from a file."))

(defgeneric get-collection (xdb name)
    (:documentation "Returns the collection by name."))

(defgeneric add-collection (xdb name &key load-from-file-p)
  (:documentation "Adds a collection to the db."))

(defgeneric snapshot (collection)
  (:documentation "Write out a snapshot."))

(defgeneric load-db (xdb &key load-from-file-p)
  (:documentation "Loads all the collections in a location."))

(defgeneric get-docs (xdb collection-name &key return-type &allow-other-keys)
  (:documentation "Returns the docs that belong to a collection."))

(defgeneric get-doc (collection value  &key element test)
  (:documentation "Returns the docs that belong to a collection."))

(defgeneric get-doc-complex (test element value collection  &rest more-collections)
  (:documentation "Returns the docs that belong to a collection."))

(defgeneric get-doc-simple (element value collection  &rest more-collections)
  (:documentation "Returns the docs that belong to a collection."))

(defgeneric find-doc (collection &key test)
  (:documentation "Returns the docs that belong to a collection."))

(defgeneric find-doc-complex (test collection &rest more-collections)
  (:documentation "Returns the first doc that matches the test."))

(defgeneric find-docs (return-type test collection))

(defgeneric union-collection (return-type collection &rest more-collections))

(defgeneric sort-collection (collection &key return-sort sort-value-func sort-test-func)
  (:documentation "This sorts the collection 'permanantly'."))

(defgeneric sort-collection-temporary (collection &key sort-value-func sort-test-func)
  (:documentation "This does not sort the actual collection but returns an array
of sorted docs."))

(defgeneric enable-sequences (xdb))

(defgeneric next-sequence (xdb key))

(defgeneric sum (collection &key function &allow-other-keys)
  (:documentation "Applies the function to all the docs in the collection and returns the sum of
the return values."))

(defgeneric max-val (collection &key function element))

;;; Document
(defgeneric add (doc &key collection duplicate-doc-p-func)
  (:documentation "Add a document to the docs container."))

;;; Disk
(defgeneric write-object (object stream))
