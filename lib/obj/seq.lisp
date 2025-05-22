;;; lib/obj/seq.lisp --- Sequences

;; This package provides CLOS mixins for implementing sequences and iterators.

;; We use SBCL's extension to ANSI spec which allows subclassing of
;; the SEQUENCE class as well as the Iterator Protocol described in
;; the manual. Where possible, we avoid the Simple Iterator Protocol.

;; SB-SEQUENCE is similar to SB-POSIX in the sense that you're
;; supposed to use their package prefixes since they conflict with
;; symbols exported by CL. This package can be USEd in a DEFPACKAGE
;; form without conflicts.

;;; Code:
(in-package :obj/seq)

(defclass iterator ()
  ()
  (:documentation "Iterator superclass inherited by objects implementing the iterator protocol."))

;;; Protocol
(defvar *idx* 0)
(let ((*idx* 0))
  (defgeneric next (self)
    (:method ((self array))
      (prog1 (aref self *idx*)
        (incf *idx*))))
  (defgeneric idx (self)
    (:method ((self t)) *idx*))
  (defgeneric prev (self)
    (:method ((self array))
      (decf *idx*)
      (aref self *idx*))))
(defgeneric key (self))
(defgeneric val (self))
(defgeneric iter (self &key &allow-other-keys))
(defgeneric iter-valid-p (self))
(defgeneric seek (self key &key))
(defgeneric seek-to-first (self))
(defgeneric seek-to-last (self))
(defgeneric seek-for-prev (self key &key))

(defvar *iter*)

(defvar *iterator-functions*
  '((next (&optional (s *iter*)) (next s))
    (prev (&optional (s *iter*)) (prev s))
    (seek-to-first (&optional (s *iter*)) (seek-to-first s))
    (seek-to-last (&optional (s *iter*)) (seek-to-last s))
    (seek-for-prev (key &optional (s *iter*)) (seek-for-prev s key))
    (iter-valid-p (&optional (s *iter*)) (iter-valid-p s))
    (seek (key &optional (s *iter*)) (seek s key))
    (val (&optional (s *iter*)) (val s))
    (key (&optional (s *iter*)) (key s))))

(defmacro with-iter ((sym iter) &body body)
    `(let ((,sym (setf *iter* ,iter)))
       (flet ,*iterator-functions*
         (declare (ignorable ,@(mapcar (lambda (x) `(function ,(car x))) *iterator-functions*)))
         ,@body)))
