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

;;;; Value
(defmethod parse-toml-value ((type (eql :datetime)) value)
  "Return a timestamp."
  (parse-timestring (ppcre:regex-replace " " "T" value)))

(defmethod parse-toml-value ((type (eql :datetime-local)) value)
  "Return a plist with keys (:year :month :day :hour :minute :second)."
  (let* ((delimeter (sequence:elt value 10))
         (splits (split-sequence delimeter value)))
    (append (parse-toml-value :date-local (car splits))
            (parse-toml-value :time-local (cadr splits)))))

(defmethod parse-toml-value ((type (eql :date-local)) value)
  "Return a plist with keys (:year :month :day)."
  (let* ((*default-timezone* +utc-zone+)
         (timestamp (parse-timestring value)))
    (list :year (timestamp-year timestamp)
          :month (timestamp-month timestamp)
          :day (timestamp-day timestamp))))

(defmethod parse-toml-value ((type (eql :time-local)) value)
  "Return a plist with keys (:hour :minute :second)."
  (let* ((*default-timezone* +utc-zone+)
         (timestamp (parse-timestring value)))
    (list :hour (timestamp-hour timestamp)
          :minute (timestamp-minute timestamp)
          :second (timestamp-second timestamp)
          :microsecond (timestamp-microsecond timestamp))))

(defmethod parse-toml-value (type value)
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

;; (defmethod serialize ((table toml-table) (format (eql :toml)) &key (style :alist))
;;   (loop with children = (children table)
;;         for key being the hash-keys of children
;;         collect (cons key (serialize (gethash key children) format :style style))))

;; (defmethod serialize ((table inline-toml-table) (format (eql :toml)) &key (style :alist))
;;   (loop with children = (children table)
;;         for key being the hash-keys of children
;;         collect (cons key (serialize (gethash key children) format :style style))))

;; (defmethod serialize ((table toml-table-array) (format (eql :toml)) &key style)
;;   (mapcar (lambda (it) (serialize it format :style style))
;;           (children table)))

;; (defmethod serialize (thing (format (eql :toml)) &key style)
;;   (declare (ignore style))
;;   thing)

;; (defmethod serialize ((thing list) (format (eql :toml)) &key)
;;   (if (listp (cdr thing))
;;       (mapcar (lambda (it) (serialize it :toml)) thing)
;;       thing))

;; (defmethod serialize (thing (format (eql :toml)) &key)
;;   thing)
