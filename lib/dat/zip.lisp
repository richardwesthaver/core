;;; zip.lisp --- Zip Files

;; 

;;; Code:
(in-package :dat/zip)

(defclass zip-file ()
  ((entries :initarg :entries :initform (make-array 0 :adjustable t :fill-pointer t) :accessor entries)
   (disks :initarg :disks :initform nil :accessor disks)
   (comment :initform nil :initarg :comment :accessor comment)))

(defclass zip-entry ()
  ((zip-file :initarg :zip-file :initform NIL :accessor zip-file)
   (crc-32 :initform nil :accessor crc-32)
   (disk :initform nil :accessor disk)
   (offset :initform nil :accessor offset)
   (size :initform nil :accessor size)
   (uncompressed-size :initform nil :accessor uncompressed-size)
   (extra-fields :initform nil :accessor extra-fields)
   (version :initform nil :initarg :version :accessor version)
   (attributes :initform nil :initarg :attributes :accessor attributes)
   (encryption-method :initform nil :initarg :encryption-method :accessor encryption-method)
   (compression-method :initform nil :initarg :compression-method :accessor compression-method)
   (last-modified :initform (get-universal-time) :initarg :last-modified :accessor last-modified)
   (file-name :initform nil :initarg :file-name :accessor file-name)
   (comment :initform nil :initarg :comment :accessor comment)
   (content :initform nil :initarg :content :accessor content)))
