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

(defun log-message-to-octets (msg)
  "Convert a LOG-MESSAGE to an OCTET-VECTOR and return it."
  (declare (optimize speed))
  (with-slots (timestamp level content) msg
    (coerce level 'octet)
    (integer-to-octets (timestamp-to-unix (now)) 64)
    (sb-ext:string-to-octets content)))

(defun octets-to-log-message (octets)
  "Convert OCTETS to a LOG-MESSAGE object.")

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
