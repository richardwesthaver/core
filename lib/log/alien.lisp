;;; alien.lisp --- Alien Logger

;; Loggers which communicate across FFI via foreign objects, functions and
;; callbacks.

;;; Commentary:

;; Various libraries wrapped in the FFI supermodule provide their own logging
;; mechanisms, some of which provide a 2-way communication channel for log
;; processing and messaging via callbacks which we must define ourselves.

;; This package defines ALIEN-LOGGER, ALIEN-SINK, and ALIEN-SOURCE classes as
;; well as utilities for working with logging APIs defined in C libs.

;;; Code:
(in-package :log)

(defclass alien-sink (stream-sink) ()
  (:default-initargs :output (make-instance 'io/static:static-stream)))

(defun log-message-to-octets (msg &key (level t))
  "Convert a LOG-MESSAGE to an OCTET-VECTOR and return it.

When WITH-LEVEL is non-nil we also encode the LEVEL slot of MSG as the last
byte. The default is T to support a database with a single column, but may be
excluded in a multi-column logger where each level has its own column.

Messages are packed as follows:
- 32-bit timestamp
- 32-bit content-size
- content array of length content-size bytes
- optional trailing byte
;; level: u8
;; content: (array u8 *)
"
  (declare (optimize speed))
  (with-slots (timestamp (l level) content) msg
    (let* ((cont (when content (sb-ext:string-to-octets content)))
           (len (integer-to-octets (length cont) 32))
           (ts (integer-to-octets (timestamp-to-unix timestamp) 32))
           (lvl (when level (list (coerce (ilevel l) 'octet)))))
      (concatenate 'octet-vector ts len cont lvl))))

(defun octets-to-log-message (octets &key (level t))
  "Convert OCTETS to a LOG-MESSAGE object."
  (declare (optimize speed)
           (octet-vector octets))
  (let ((ts (unix-to-timestamp (octets-to-integer (subseq octets 0 4) 4)))
        (len (octets-to-integer (subseq octets 4 8) 4))
        (pos 8)
        (obj (make-instance 'log-message)))
    (setf (timestamp obj) ts
          (level obj) (when level (aref octets len))
          (content obj) (octets-to-string octets :start pos :end (+ pos len)))
    obj))

(defmethod msg ((elt alien-sink) (msg log-message)))

(defmethod msg ((elt alien-sink) (msg simple-message)))

(defclass alien-source (stream-source) ()
  (:default-initargs :input (make-instance 'io/static:static-stream)))

(defclass alien-logger (logger) ())

(defun make-alien-logger (&rest args)
  (let ((pipe (apply 'make-instance 'logger args)))
    (defpipe (pipe)
      (level-filter :id 'alien-level)
      (tag-tree-filter :id 'alien-tags)
      (alien-sink :id 'alien-stream))))
