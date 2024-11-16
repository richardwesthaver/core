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

(defclass alien-sink (sink) ())
(defmethod msg ((elt alien-sink) (msg log-message)))
(defmethod msg ((elt alien-sink) (msg simple-message)))

(defclass alien-source (source) ())

(defclass alien-logger (logger) ())
