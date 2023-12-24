(in-package :dat/toml)

;;; Errors
(define-condition toml-parse-error (error) ())

(define-condition toml-invalid-text-error (toml-parse-error)
  ((text :accessor text :initarg :text))
  (:report (lambda (condition stream)
             (format stream
                     "Invalid text ~a detected"
                     (text condition)))))

(define-condition toml-invalid-utf8-error (toml-invalid-text-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Invalid utf8 ~a detected"
                     (text condition)))))

(define-condition toml-table-error (error)
  ((names :accessor names :initarg :names)))

(define-condition toml-redefine-table-error (toml-table-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Table name ~a is already defined"
                     (names condition)))))

(define-condition toml-redefine-property-error (toml-table-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Property name ~a is already defined"
                     (names condition)))))

(define-condition toml-modify-inline-table-error (toml-table-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Inline table ~a cannot be modified once defined"
                     (names condition)))))

(define-condition toml-dotted-key-redefine-table-error (toml-table-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Dotted key ~a cannot redefine table defined by [Table] header or dotted key from another section"
                     (names condition)))))

(define-condition toml-dotted-key-open-table-array-error (toml-table-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "Dotted key ~a cannot open table array"
                     (names condition)))))

;;; Vars
(defvar *toml-value-+inf* :+inf
  "The value of +inf when decoding TOML.")

(defvar *toml-value--inf* :-inf
  "The value of -inf when decoding TOML.")

(defvar *toml-value-+nan* :+nan
  "The value of +nan when decoding TOML.")

(defvar *toml-value--nan* :-nan
  "The value of -nan when decoding TOML.")

(defvar *toml-value-true* t
  "The value of true when decoding TOML.")

(defvar *toml-value-false* nil
  "The value of false when decoding TOML.")

;;; Block
(defclass toml-block () ())

(defclass toml-key-value-pair (toml-block)
  ((keys :type list :initarg :keys :accessor keys)
   (value :accessor value :initarg :value)))

(defclass toml-table (toml-block) ())

(defclass toml-named-table (toml-table)
  ((names :type string :initarg :names :accessor names)))

(defclass toml-inline-table (toml-table)
  ((pairs :type list :initarg :pairs :accessor pairs)))

(defclass toml-array-table (toml-named-table) ())

(defmethod print-object ((obj toml-key-value-pair) stream)
  (format stream "#toml-key-value-pair (~a . ~a)"
          (keys obj)
          (value obj)))

(defmethod print-object ((table toml-named-table) stream)
  (format stream "#toml-named-table (~a)" (names table)))

(defmethod print-object ((table toml-inline-table) stream)
  (format stream "#toml-inline-table (~a)" (pairs table)))

(defmethod print-object ((table toml-array-table) stream)
  (format stream "#toml-array-table (~a)" (names table)))

;;; Model
(defclass collection ()
  ((children :accessor children
             :initform (make-hash-table :test #'equal)
             :documentation "A table of any kind. Note that for a table, its own name is not stored as a property of itself, but as a hash key in children property of its parent collection. The parsed result is a table representing root table.")))

(defclass table (collection)
  ((definition-context :type boolean
                       :accessor definition-context
                       :initarg :definition-context
                       :initform nil
                       :documentation "Internal use.

Indicates if the table is defined or not.
A table is defined in the following ways:
1. By [Table] header.
2. By being a path of dotted.key.tables. In this case, all the tables along the
way are created and defined.

Its value can be:
- NIL means table is opened but not defined.
- T means defined via [Table] header.
- A table instance means defined under corresponding table section.")))

(defclass inline-table (collection) ())

(defclass table-array (collection)
  ((children :initform (list))))

(defmethod print-object ((table table) stream)
  (format stream "#Table(~{ ~S~})"
          (hash-table-alist (children table))))

(defmethod print-object ((table inline-table) stream)
  (format stream "#InlineTable(~{ ~S~})"
          (hash-table-alist (children table))))

(defmethod print-object ((table table-array) stream)
  (format stream "#ArrayTable(~{ ~S~})"
          (children table)))

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
                   (table (if last-name-p
                              (progn (if (definition-context table)
                                         (error 'toml-redefine-table-error
                                                :names names)
                                         (setf (definition-context table) t))
                                     (setf (current-table context) table))
                              (setf current-table table)))
                   (table-array (if last-name-p
                                    (error 'toml-redefine-table-error
                                           :names names)
                                    (setf current-table (last-child table))))
                   (t (error 'toml-redefine-table-error :names names)))
                 (let ((table (make-instance 'table)))
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
                   (table (if last-name-p
                              (error 'toml-redefine-table-error :names names)
                              (setf current-table table)))
                   (table-array (if last-name-p
                                    (let ((new-table (make-instance 'table)))
                                      (append-child table new-table)
                                      (setf (current-table context) new-table))
                                    (setf current-table (last-child table))))
                   (t (error 'toml-redefine-table-error :names names)))
                 (if last-name-p
                     ;; For last part of names, create table array.
                     (let ((table (make-instance 'table))
                           (table-array (make-instance 'table-array)))
                       (set-child current-table name table-array)
                       (append-child table-array table)
                       (setf (current-table context) table))
                     ;; For middle part of names, create normal table.
                     (let ((table (make-instance 'table)))
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
                                     'table
                                     :definition-context current-table)))
                     (set-child table key new-table)
                     (setf table new-table)))
          else
            do (if last-name-p
                   (error 'toml-redefine-property-error :names keys)
                   (case (type-of value)
                     (table (if (equal (definition-context value)
                                       current-table)
                                (setf table value)
                                (error 'toml-dotted-key-redefine-table-error
                                       :names keys)))
                     (inline-table (error 'toml-modify-inline-table-error
                                          :names keys))
                     (table-array (error 'toml-dotted-key-open-table-array-error
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
     (let ((inline-table (make-instance 'inline-table))
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
         (splits (split delimeter value)))
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

;;; API

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

(defmethod serialize ((table table) (format (eql :toml)) &key (style :alist))
  (loop with children = (children table)
        for key being the hash-keys of children
        collect (cons key (serialize (gethash key children) format :style style))))

(defmethod serialize ((table inline-table) (format (eql :toml)) &key (style :alist))
  (loop with children = (children table)
        for key being the hash-keys of children
        collect (cons key (serialize (gethash key children) format :style style))))

(defmethod serialize ((table table-array) (format (eql :toml)) &key style)
  (mapcar (lambda (it) (serialize it format :style style))
          (children table)))

(defmethod serialize (thing (format (eql :toml)) &key style)
  (declare (ignore style))
  thing)

(defmethod serialize ((thing list) (format (eql :toml)) &key style)
  (if (listp (cdr thing))
      (mapcar (lambda (it) (serialize it style)) thing)
      thing))

(defmethod serialize (thing (format (eql :toml)) &key (style :raw))
  (declare (ignore style))
  thing)
