;;; dm.lisp --- Direct Message Protocol

;; A simple TLV protocol for stateless p2p communication.

;;; Commentary:

;; Peers are 

;;; Code:
(in-package :net/proto/dm)

(defvar *peers* (make-hash-table :test 'equal))

(defvar *peer* nil)

(defun get-peer (name)
  (gethash (keywordicate name) *peers*))

(defun (setf get-peer) (addr name)
  (declare (keyword name))
  (setf (gethash name *peers*) addr))

(defvar *dm-node* nil)

(defclass dm-node (udp-client udp-server worker) ())

(defmethod initialize-instance :after ((self dm-node) &key)
  (setq *dm-node* self))

(defun dm (peer dm)
  (let ((buffer (serialize dm :tlv)))
    (socket-send *dm-node* buffer (length buffer) :address peer)))

(defun ensure-dm-node ()
  (unless *dm-node* (make-instance 'dm-node)))

(defmacro with-peer (peer &body body)
  `(let ((,peer (get-peer ',peer)))
     (ensure-dm-node)
     ,@body))

(defmacro with-peers ((&rest peers) &body body)
  `(let (,@(mapcar (lambda (p) `(,p (get-peer ',p))) peers))
     (ensure-dm-node)
     ,@body))
