;;; dat/toml.lisp --- TOML

;; TOML de/serialization for Lisp.

;;; Commentary:

;; This code was originally based on https://github.com/sheepduke/clop which
;; provides a TOML parser using the ESRAP package.

;; ref: https://toml.io/en/v1.0.0

;; grammar: https://raw.githubusercontent.com/toml-lang/toml/1.0.0/toml.abnf

#|
* TOML is case-sensitive.                                    
* A TOML file must be a valid UTF-8 encoded Unicode document.
* Whitespace means tab (0x09) or space (0x20).               
* Newline means LF (0x0A) or CRLF (0x0D 0x0A).               
|#

;;; Code:
(in-package :dat/toml)

;;; Vars
(defvar *+inf* :+inf
  "The value of +inf when decoding TOML.")

(defvar *-inf* :-inf
  "The value of -inf when decoding TOML.")

(defvar *+nan* :+nan
  "The value of +nan when decoding TOML.")

(defvar *-nan* :-nan
  "The value of -nan when decoding TOML.")

(defclass toml-object () ())

(defclass toml-table (toml-object)
  ((table :initform (make-hash-table :test 'equal))))

(defclass toml-inline-table (toml-table) ())

(defclass toml-array-table (toml-table) ())

(defclass toml-value (toml-object) ())

(defclass toml-pair ()
  ((key :type (or symbol string) :initarg :key) (val :type toml-value :initarg :val)))

;;; Collections
(defclass toml-document ()
  ((children :accessor children
             :type (or list (vector toml-object))
             :documentation "A table of any kind. Note that for a table, its own name is not stored as a
property of itself, but as a hash key in children property of its parent
collection. The parsed result is a table representing root table.")))

;;; Read
(defun toml-read (stream &optional (eof-error-p t) eof-value)
  (let ((c (peek-char t stream eof-error-p :eof)))
    (case c
      (:eof eof-value)
      (#\[ (read-char stream) (toml-read-header stream)) ;; arrays are values only
      (t (toml-read-key stream)))))

(defun toml-peek-char (stream expected &key skip-ws)
  (when (equal (peek-char skip-ws stream) expected)
    (read-char stream)))

(defun toml-read-header (stream)
  (let ((c (peek-char t stream nil nil)))
    (case c
      ;; array-table
      (#\[ (read-char stream) (toml-read-key stream))
      (t (toml-read-key stream)))))

(defun toml-read-key (stream))

(defun toml-read-value (stream))

(defun toml-read-pair (stream))

;;; Parser
(defclass parser-context ()
  ((root-table :reader root-table
               :initform (make-instance 'table))
   (current-table :accessor current-table :initform nil)))

;;;; Block
(defun parse-toml-blocks (list)
  "Given a LIST of components (tables or key-value pairs), return an alist."
  (let ((context (make-instance 'parser-context)))
    (setf (current-table context) (root-table context))
    (mapc (lambda (toml-block) (parse-toml-block toml-block context)) list)
    (root-table context)))

(defgeneric parse-toml-block (toml-block context))

(defmethod parse-toml-block ((toml-table toml-named-table) context)
  (loop with names = (names toml-table)
        with length = (length names)
        with current-table = (root-table context)
        for name in names
        for i from 1
        for last-name-p = (= i length)
        do (multiple-value-bind (table table-found-p)
               (get-child current-table name)
             (if table-found-p
                 (case (type-of table)
                   (toml-table (if last-name-p
                              (progn (if (definition-context table)
                                         (error 'toml-redefine-table-error
                                                :names names)
                                         (setf (definition-context table) t))
                                     (setf (current-table context) table))
                              (setf current-table table)))
                   (toml-table-array (if last-name-p
                                    (error 'toml-redefine-table-error
                                           :names names)
                                    (setf current-table (last-child table))))
                   (t (error 'toml-redefine-table-error :names names)))
                 (let ((table (make-instance 'toml-table)))
                   (when last-name-p
                     (setf (definition-context table) t)
                     (setf (current-table context) table))
                   (set-child current-table name table)
                   (setf current-table table))))))

(defmethod parse-toml-block ((toml-table toml-array-table) context)
  (loop with names = (names toml-table)
        with length = (length names)
        with current-table = (root-table context)
        for name in names
        for i from 1
        for last-name-p = (= i length)
        do (multiple-value-bind (table table-found-p)
               (get-child current-table name)
             (if table-found-p
                 (case (type-of table)
                   (toml-table (if last-name-p
                              (error 'toml-redefine-table-error :names names)
                              (setf current-table table)))
                   (toml-table-array (if last-name-p
                                    (let ((new-table (make-instance 'toml-table)))
                                      (append-child table new-table)
                                      (setf (current-table context) new-table))
                                    (setf current-table (last-child table))))
                   (t (error 'toml-redefine-table-error :names names)))
                 (if last-name-p
                     ;; For last part of names, create table array.
                     (let ((table (make-instance 'toml-table))
                           (table-array (make-instance 'toml-table-array)))
                       (set-child current-table name table-array)
                       (append-child table-array table)
                       (setf (current-table context) table))
                     ;; For middle part of names, create normal table.
                     (let ((table (make-instance 'toml-table)))
                       (set-child current-table name table)
                       (setf current-table table)))))))

(defmethod parse-toml-block ((pair toml-key-value-pair) context)
  (let* ((current-table (current-table context))
         (table current-table)
         key-to-add value-to-add)
    ;; Parse keys.
    (loop with keys = (keys pair)
          with length = (length keys)
          for key in keys
          for i from 1
          for last-name-p = (= i length)
          for value = (get-child table key)
          if (null value)
            do (if last-name-p
                   (setf key-to-add key)
                   (let ((new-table (make-instance
                                     'toml-table
                                     :definition-context current-table)))
                     (set-child table key new-table)
                     (setf table new-table)))
          else
            do (if last-name-p
                   (error 'toml-redefine-property-error :names keys)
                   (case (type-of value)
                     (toml-table (if (equal (definition-context value)
                                       current-table)
                                (setf table value)
                                (error 'toml-dotted-key-redefine-table-error
                                       :names keys)))
                     (inline-toml-table (error 'toml-modify-inline-table-error
                                          :names keys))
                     (toml-table-array (error 'toml-dotted-key-open-table-array-error
                                         :names keys))
                     (t (error 'toml-redefine-property-error
                               :names keys)))))

    (setf value-to-add
          (parse-pair-value (value pair) context))

    ;; Parse value.
    (set-child table
               key-to-add
               value-to-add
               ;; (parse-pair-value (value pair) context)
               )))

(defun parse-pair-value (value context)
  (cond
    ((typep value 'toml-inline-table)
     (let ((inline-table (make-instance 'toml-inline-table))
           (current-table (current-table context)))
       (setf (current-table context) inline-table)
       (parse-toml-block value context)
       (setf (current-table context) current-table)
       inline-table))
    ((and (listp value)
          (listp (cdr value)))
     (mapcar (lambda (v) (parse-pair-value v context)) value))
    (t value)))

(defmethod parse-toml-block ((toml-table toml-inline-table) context)
  (loop for pair in (pairs toml-table)
        do (parse-toml-block pair context)))

(defun append-child (table-array table)
  "Append TABLE as a child to TABLE-ARRAY."
  (appendf (children table-array) (list table)))

(defun last-child (table-array)
  "Get the last child of TABLE-ARRAY."
  (first (last (children table-array))))

(defun set-child (table name value)
  "Set the child of TABLE specified by NAME to VALUE."
  (setf (gethash name (children table)) value))

(defun get-child (table name)
  "Get the child of TABLE specified by NAME."
  (gethash name (children table)))

;;;; Value
(defgeneric parse-value (type value))

(defmethod parse-value ((type (eql :datetime)) value)
  "Return a timestamp."
  (parse-timestring (ppcre:regex-replace " " "T" value)))

(defmethod parse-value ((type (eql :datetime-local)) value)
  "Return a plist with keys (:year :month :day :hour :minute :second)."
  (let* ((delimeter (sequence:elt value 10))
         (splits (split-sequence delimeter value)))
    (append (parse-value :date-local (car splits))
            (parse-value :time-local (cadr splits)))))

(defmethod parse-value ((type (eql :date-local)) value)
  "Return a plist with keys (:year :month :day)."
  (let* ((*default-timezone* +utc-zone+)
         (timestamp (parse-timestring value)))
    (list :year (timestamp-year timestamp)
          :month (timestamp-month timestamp)
          :day (timestamp-day timestamp))))

(defmethod parse-value ((type (eql :time-local)) value)
  "Return a plist with keys (:hour :minute :second)."
  (let* ((*default-timezone* +utc-zone+)
         (timestamp (parse-timestring value)))
    (list :hour (timestamp-hour timestamp)
          :minute (timestamp-minute timestamp)
          :second (timestamp-second timestamp)
          :microsecond (timestamp-microsecond timestamp))))

(defmethod parse-value (type value)
  value)

;;; Serde

;; TODO 2023-12-23: 

;; (defun parse (text &key (style :alist))
;;   "Parse given string TEXT and convert the result to given STYLE.
;; The STYLE can be one of:
;; * :alist (the default)
;; * :raw (should be rarely used)

;; The top-level of result is an alist.

;; You may implement your own style by implementing SERIALIZE method."
;;   (let* ((parsed (esrap:parse 'toml text)))
;;     (serialize parsed style)))

(defmethod serialize ((table toml-table) (format (eql :toml)) &key (style :alist))
  (loop with children = (children table)
        for key being the hash-keys of children
        collect (cons key (serialize (gethash key children) format :style style))))

(defmethod serialize ((table inline-toml-table) (format (eql :toml)) &key (style :alist))
  (loop with children = (children table)
        for key being the hash-keys of children
        collect (cons key (serialize (gethash key children) format :style style))))

(defmethod serialize ((table toml-table-array) (format (eql :toml)) &key style)
  (mapcar (lambda (it) (serialize it format :style style))
          (children table)))

(defmethod serialize (thing (format (eql :toml)) &key style)
  (declare (ignore style))
  thing)

(defmethod serialize ((thing list) (format (eql :toml)) &key)
  (if (listp (cdr thing))
      (mapcar (lambda (it) (serialize it :toml)) thing)
      thing))

(defmethod serialize (thing (format (eql :toml)) &key)
  thing)
