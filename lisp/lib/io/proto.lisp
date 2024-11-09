;;; io/proto.lisp --- IO Protocol

;; 

;;; Code:
(in-package :io/proto)

(deferror io-error () () (:auto t))

(defgeneric input (self))
(defgeneric (setf input) (new self))
(defgeneric input-size (self))
(defgeneric (setf input-size) (new self))
(defgeneric output (self))
(defgeneric (setf output) (new self))
(defgeneric (setf output-size) (new self))
(defgeneric input-buffer (self))
(defgeneric (setf input-buffer) (new self))
(defgeneric output-buffer (self))
(defgeneric (setf output-buffer) (new self))
(defgeneric input-position (self))
(defgeneric (setf input-position) (new self))`
(defgeneric output-position (self))
(defgeneric (setf output-position) (new self))
(defgeneric input-available-p (self))
(defgeneric output-available-p (self))
(defgeneric fill-buffer (self))
(defgeneric header (self))
(defgeneric header-type (self))
(defgeneric header-length (self))
