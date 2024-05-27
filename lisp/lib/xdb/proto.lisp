(in-package :xdb)

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

(defgeneric get-collection (db name)
  (:documentation "Returns the collection by name."))

(defgeneric add-collection (db name &key load-from-file-p)
  (:documentation "Adds a collection to the db."))

(defgeneric snapshot (collection)
  (:documentation "Write out a snapshot."))

(defgeneric load-db (db &key load-from-file-p)
  (:documentation "Loads all the collections in a location."))

(defgeneric get-docs (db collection-name &key return-type &allow-other-keys)
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

(defgeneric sum (collection &key function &allow-other-keys)
  (:documentation "Applies the function to all the docs in the collection and returns the sum of
the return values."))

(defgeneric max-val (collection &key function element))

;;; Document
(defgeneric add (doc &key collection duplicate-doc-p-func)
  (:documentation "Add a document to the docs container."))

;;; Disk
(defgeneric write-object (object stream))
