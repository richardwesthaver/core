;;; pipe.lisp --- Simple Pipelines

;; Pipeline classes

;;; Commentary:

;; based loosely on Shinmera's PIPING library, aligning more with GStreamer's
;; terminology and notion of pipelines.

;; https://github.com/Shinmera/piping

;; https://gstreamer.freedesktop.org/documentation/application-development/introduction/basics.html?gi-language=c

;; TODO 2024-10-13: events, queries, message-bus, buffers

;;; Code:
(in-package :std/pipe)

(define-condition unknown-element ()
  ((name :initarg :name :reader name)
   (pipe :initarg :pipe :reader pipe))
  (:documentation "Error signaled when a PIPE is queried for an unrecognized element.")
  (:report (lambda (c st)
             (format st "Element ~A is not known in pipe ~A" (name c) (pipe c)))))

(defun make-pipe ()
  "Creates a new adjustable pipe (array)."
  (make-array 1 :adjustable t :fill-pointer 0))

(defclass element () ())
(defmethod print-object ((obj element) stream)
  (format stream "[~a]" (type-of obj))
  obj)

(defclass sink (element) ())
(defmethod print-object ((obj sink) stream)
  (format stream ">>~a" (type-of obj))
  obj)

(defclass stream-sink (sink)
  ((output :initarg :output :initform nil :accessor output)))

(defclass file-sink (stream-sink)
  ((file :accessor file)))

(defmethod initialize-instance :after ((obj file-sink) &key file)
  (setf (file obj) file))

(defmethod (setf file) (file (obj file-sink))
  (with-slots (output) obj
    (when output
      (close output))
    (when file
      (setf output (open file :direction :output
                              :if-exists :append
                              :if-does-not-exist :create
                              :external-format :utf-8))
      (setf (slot-value obj 'file) file))))

(defclass source (element) ())
(defmethod print-object ((obj source) stream)
  (format stream "~a>>" (type-of obj))
  obj)

(defclass stream-source (source)
  ((input :initarg :input :initform nil :accessor input)))

(defclass filter (element) ())
(defmethod print-object ((obj filter) stream)
  (format stream ":~a:" (type-of obj))
  obj)

(defclass bin (element) ())

(defclass predicate-filter (filter)
  ((predicate :initarg :predicate :accessor predicate)))

(defclass print-filter (filter)
  ((stream :initarg :stream :initform *standard-output* :accessor element-stream))
  (:documentation "A filter that prints and returns messages."))

(defclass switch-filter (filter)
  ((value :initarg :value :initform 0 :accessor value)
   (pipe :initarg :pipe :accessor pipe)))

(defclass pipe () 
  ((pipe :initarg :pipe :initform (make-pipe) :accessor pipe)
   (index :initarg :index :initform (make-hash-table :test 'eql) :accessor index)))

(defgeneric resolve-element (pipe path &key if-does-not-exist)
  (:method ((pipe pipe) (path list) &key &allow-other-keys)
    (values path t))
  (:method ((pipe pipe) (path symbol) &key (if-does-not-exist :error))
    (or (gethash path (index pipe))
        (ecase if-does-not-exist
          (:error (restart-case (error 'unknown-element-name :name path :pipe pipe)
                    (use-value (value) value)))
          ((nil) (values nil nil))))))

(defgeneric find-element (elt path)
  (:method ((elt element) path)
    (if path
        (error "Cannot descend into element")
        elt))
  (:method ((array array) (path list))
    (labels ((%find (array p) (if p (%find (aref array (pop path)) path) array)))
      (values (%find array path) path)))
  (:method ((elt pipe) (path symbol))
    (if path
        (find-element elt (resolve-element elt path))
        (call-next-method)))
  (:method ((elt switch-filter) path)
    (find-element (pipe elt) path)))
  
(defgeneric find-parent-element (elt path)
  (:method ((elt element) path)
    (declare (ignore path))
    (error "Cannot descend into element"))
  (:method ((array array) (path list))
    (if (<= (length path) 1)
        (values array (car path))
        (find-parent-element (aref array (pop path)) path)))
  (:method ((elt pipe) (path list))
    (find-parent-element (pipe elt) path))
  (:method ((elt pipe) (path symbol))
    (if path
        (find-parent-element elt (resolve-element elt path))
        (call-next-method)))
  (:method ((elt switch-filter) path)
    (find-parent-element (pipe elt) path)))

(defgeneric insert-element (elt pipe &optional pos)
  (:method (elt (pipe array) &optional pos)
    (if pos
        (vector-push-extend-position elt pipe pos)
        (vector-push-extend elt pipe))
    elt)
  (:method (elt (pipe switch-filter) &optional pos)
    (insert-element elt (pipe pipe) pos)))

(defgeneric withdraw-element (pipe &optional pos)
  (:method ((pipe array) &optional pos)
    (if pos
        (vector-pop-position pipe pos)
        (vector-pop pipe)))
  (:method ((pipe switch-filter) &optional pos)
    (withdraw-element (pipe pipe) pos)))

(defgeneric remove-element (pipe elt)
  (:method ((pipe pipe) elt)
    (prog1
        (multiple-value-bind (parent pos) (find-parent-element pipe elt)
          (withdraw-element parent pos))
      (loop with parent = (subseq elt 0 (1- (length elt)))
            with pos = (car (last elt))
            for k being the hash-keys of (index pipe)
            for v being the hash-values of (index pipe)
            when (and (<= (length elt) (length v))
                      (every #'= elt v))
              do (remhash k (index pipe))
            when (and (<= (length parent) (length v))
                      (every #'= parent v)
                      (< pos (nth (length parent) v)))
              do (decf (nth (length parent) v))))))

(defgeneric set-element-id (pipe path name)
  (:method ((pipe pipe) (path list) (name symbol))
    (setf (gethash name (index pipe)) path)))

(defgeneric move-element (pipe elt new-elt)
  (:method ((pipe pipe) elt new-elt)
    (prog1
        (let ((e (remove-element pipe elt)))
          (insert-element pipe e new-elt))
      (loop for k being the hash-keys of (index pipe)
            for v being the hash-values of (index pipe)
            when (and (<= (length elt) (length v))
                      (every #'= elt v))
            do (set-element-id pipe (append elt (subseq v (length elt))) k)))))

(defgeneric msg (elt msg)
  (:method ((elt pipe) msg)
    (msg (pipe elt) msg))
  (:method ((elt vector) msg)
    (let ((msg msg))
      (loop for i across elt
            do (setf msg (msg i msg))
            while msg))
    msg)
  (:method ((elt element) msg)
    msg)
  (:method ((elt predicate-filter) msg)
    (when (funcall (predicate elt) msg)
      msg))
  (:method ((elt print-filter) msg)
    (print msg (element-stream elt))
    msg)
  (:method ((elt switch-filter) msg)
    (msg (aref (pipe elt) (value elt)) msg))
  (:method ((elt stream-sink) msg)
    (when (output elt)
      (format-message elt msg))
      msg))

(defclass message () ())
(defclass event () ())
(defclass buffer () ())
(defclass bus () ())

(defgeneric format-message (stream message)
  (:method ((stream null) message)
    (princ-to-string message))
  (:method :before ((stream stream) message)
    (fresh-line stream))
  (:method ((stream t) message)
    (format-message *standard-output* message))
  (:method ((stream null) (message function))
    (princ-to-string (funcall message)))
  (:method ((stream null) (message message))
    (with-output-to-string (stream)
      (format-message stream message)))
  (:method :before ((stream stream-sink) message)
    (fresh-line (output stream)))
  (:method :after ((stream stream-sink) message)
    (terpri (output stream))
    (force-output (output stream)))
  (:method ((stream stream-sink) (message message))
    (format-message (output stream) message)))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type t)
    (format-message stream message)))

(defclass condition-message (message)
  ((condition :initarg :condition
              :initform (required-argument "CONDITION")
              :accessor message-condition)))
