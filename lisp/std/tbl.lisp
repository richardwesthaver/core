;;; tbl.lisp --- Simple table data structures.

;;; Code:

(defpackage :std/tbl
  (:use :cl :std/base)
  (:import-from :uiop :split-string)
  (:export 
   :make-table
   :make-row
   :add-to-table
   :add-to-row
   :get-row
   :get-row-column
   :set-row-column
   :num-rows
   :num-cols
   :num-col
   :rectangular-table-p
   :sequence->row
   :row-sequence->table
   :with-rows
   :select
   :distinct
   :top
   :order-by
   :where
   :where-filter
   :where-or
   :where-and
   :read-csv
   :read-tsv
   :table-from-file))

(in-package :std/tbl)

;;; Table
(deftype row ()
  "Table row type."
  `(vector t *))

(deftype table ()
  "Table type."
  `(vector row *))

(defun make-table ()
  "Creates a table."
  (make-array 1 :element-type 'row :fill-pointer 0 :adjustable t))

(defun make-row ()
  "Create a row."
  (make-array 1 :fill-pointer 0 :adjustable t))

(defun add-to-table (row table)
  "Appends a row to the table."
  (vector-push-extend row table)
  table)

(defun add-to-row (value row)
  "Append a column to row and set it to the given value."
  (vector-push-extend value row)
  row)

(defun get-row (index table)
  "Returns the row in the given index inside the table."
  (elt table index))

(defun get-row-column (column row)
  "Gets the value in the given column inside row."
  (elt row column))

(defun set-row-column (column value row)
  "Sets the value of the given column inside the row."
  (setf (elt row column) value)
  row)

(defun num-rows (table)
  "Returns the number of rows in the table."
  (length table))

(defun num-cols (row)
  "Returns the number of elements in this row."
  (length row))

(defun rectangular-table-p (table)
  "Returns true if all the rows in the table have the same number of elements."
  (or (= (num-rows table) 0)
      (let ((cols (num-cols (get-row 0 table))))
        (every (lambda (row)
                 (eql (num-cols row) cols))
               table))))

(defun sequence->row (elements)
  "Converts a sequence of elements into a table row."
  (coerce elements 'row))

(defun row-sequence->table (rows)
  "Converts a sequence of rows into a table."
  (coerce rows 'table))

(defmacro with-rows ((table row-var &optional return-expression) &body body)
  "Iterates the rows in the given table, row-var is the current row, returning return-expression."
  (let ((iterator (gensym)))
    `(dotimes (,iterator (num-rows ,table) ,return-expression)
       (let ((,row-var (get-row ,iterator ,table)))
         ,@body))))

;;; Queries
(defun select (table &rest columns)
  "Selects the given columns from the table and returns them as a new table."
  (let ((result (make-table)))
    (with-rows (table row result)
      (let ((new-row (make-row)))
        (mapc (lambda (col)
                (add-to-row (get-row-column col row) new-row))
              columns)
        (add-to-table new-row result)))))

(defun distinct (table column)
  "Returns the unique elements from the given column in the given table as a new table."
  (let ((added (make-hash-table :test #'equal))
        (result (make-table)))
    (with-rows (table row result)
      (let ((value (get-row-column column row)))
        (unless (gethash value added)
          (let ((new-row (make-row)))
            (setf (gethash value added) t)
            (add-to-row value new-row)
            (add-to-table new-row result)))))))

(defun top (table n)
  "Returns a new table with the top n rows from the given table."
  (let ((how-many (min n (num-rows table))))
    (subseq table 0 how-many)))

(defun order-by (table col op)
  "Returns a new table sorted by the value in the given column and table using op."
  (sort table op :key (lambda (row) (get-row-column col row))))

(defun where (table filter)
  "Filters the result of the table using the given filter, returns a new table. Filters
   the result of the table using the given filter, returns a new table. Filter should be
   a predicate that takes a row and decides whether to include it in the result or not.
   Although the filter can be created by hand it is easier to use where-filter, where-and
    and where-or."
  (remove-if-not filter
                 table))

(defun where-filter (op column value)
  "Returns a filter applicable for where, it calls op to compare the given value and the
   value stored in column for every row. Besides calling op the filter returned will also
   check the type of the values are the same before being compared."
  (let ((value-type (type-of value)))
    (lambda (row)
      (let ((val (get-row-column column row)))
        (and (typep val value-type)
             (funcall op value (get-row-column column row)))))))

(defun where-or (&rest filters)
  "Given a list of filters created by where-filter this returns true if any of them is true."
  (lambda (row) (some (lambda (filter)(funcall filter row))
                      filters)))

(defun where-and (&rest filters)
  "Given a list of filters created by where-filter this returns true if all of them are true."
  (lambda (row) (every (lambda (filter) (funcall filter row))
                       filters)))

;;; Importers
(defun table-from-file (filename &key (separator #\tab) parse-elements)
  "Reads the tabular data file and returns the contents. Separator is TAB by default.
   If parse-elements is other than NIL elements from the table will be READ into Lisp objects,
   otherwise only strings will be created."
  (let ((filter (if parse-elements
                    (lambda (ln) (mapcar (lambda (el) (read-from-string el nil))
                                         (split-string separator ln)))
                    (lambda (ln) (split-string separator ln)))))
    (with-open-file (s filename :if-does-not-exist nil)
      (row-sequence->table
       (loop
          for line = (read-line s nil nil)
          until (null line)
          collect (sequence->row (funcall filter line)))))))

(defun read-csv (filename &optional parse-elements)
  "Creates a table from a comma-separated values file."
  (table-from-file filename :separator #\, :parse-elements parse-elements))

(defun read-tsv (filename &optional parse-elements)
  "Creates a table from a tab-separated values file."
  (table-from-file filename :separator #\tab :parse-elements parse-elements))
