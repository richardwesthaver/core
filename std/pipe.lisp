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

(define-condition invalid-event (invalid-item) ()
  (:documentation "Error signaled when an EVENT object is not recognized."))

(defun make-pipe ()
  "Creates a new adjustable pipe (array)."
  (make-array 1 :adjustable t :fill-pointer 0))

(defclass element () ())
(defmethod print-object ((obj element) stream)
  (format stream "[~a]" (type-of obj))
  obj)

(defgeneric sink (self))
(defclass sink (element) ())
(defmethod print-object ((obj sink) stream)
  (format stream ">>~a" (type-of obj))
  obj)

(defclass stream-sink (sink)
  ((output :initarg :output :initform (make-synonym-stream '*standard-output*) :accessor output)))

(defclass file-sink (stream-sink)
  ((file :accessor file))
  (:default-initargs
   :output nil))

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

(defgeneric source (self))
(defclass source (element) ())
(defmethod print-object ((obj source) stream)
  (format stream "~a>>" (type-of obj))
  obj)

(defclass stream-source (source)
  ((input :initarg :input :initform (make-synonym-stream '*standard-input*) :accessor input)))

(defclass file-source (stream-source)
  ((file :accessor file))
  (:default-initargs :input nil))

(defmethod initialize-instance :after ((obj file-source) &key file)
  (setf (file obj) file))

(defmethod (setf file) (file (obj file-source))
  (with-slots (input) obj
    (when input
      (close input))
    (when file
      (setf input (open file :direction :input :element-type 'octet)
            (slot-value obj 'file) file))))

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

(defgeneric find-element (elt self)
  (:method (elt (self element))
    (if elt
        (error "Cannot descend into element")
        elt))
  (:method ((elt list) (self array))
    (labels ((%find (array p) (if p (%find (aref array (pop elt)) elt) array)))
      (values (%find self elt) elt)))
  (:method ((elt symbol) (self pipe))
    (if elt
        (find-element elt (resolve-element self elt))
        (call-next-method)))
  (:method (elt (self switch-filter))
    (find-element elt (pipe self)))
  (:method ((elt integer) (self array))
    (aref self elt))
  (:method ((elt integer) (self pipe))
    (aref (pipe self) elt))
  (:method ((elt list) (self pipe))
    (find-element elt (pipe self))))

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

(defgeneric insert-element* (elt pipe &optional pos)
  (:method (elt (pipe array) &optional pos)
    (if pos
        (vector-push-extend-position elt pipe pos)
        (vector-push-extend elt pipe))
    elt)
  (:method (elt (pipe switch-filter) &optional pos)
    (insert-element* elt (pipe pipe) pos)))

(defgeneric withdraw-element (pipe &optional pos)
  (:method ((pipe array) &optional pos)
    (if pos
        (vector-pop-position pipe pos)
        (vector-pop pipe)))
  (:method ((pipe switch-filter) &optional pos)
    (withdraw-element (pipe pipe) pos)))

(defgeneric add-element (self elt &optional place)
  (:documentation "Add a new element to the pipe.
If place is set, the element is added to the specified place as per INSERT-ELEMENT*. Return the segment.")
  (:method ((self pipe) (elt element) &optional place)
    (insert-element* elt (find-element place self)))
  (:method ((self pipe) (elt array) &optional place)
    (insert-element* elt (find-element place self)))
  (:method ((self pipe) (elt (eql :pipe)) &optional place)
    (insert-element* (make-pipe) (find-element place self)))
  (:method ((self pipe) elt &optional place)
    (add-element self elt
                 (if (and place (symbolp place)
                          (resolve-element self place))
                     place))))

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

(defgeneric insert-element (self elt place)
  (:documentation "Insert the segment at the given place.
Note that the segment is always inserted into the parent as specified by the place
and found by FIND-PARENT and inserted into the position as per INSERT. PLACE can be a
name, as in FIND-ELEMENT.

Returns the segment.")
  (:method ((self pipe) (elt element) place)
    (prog1
        (multiple-value-bind (parent pos) (find-parent-element self place)
          (insert-element* elt parent pos))
      (loop with parent = (subseq place 0 (1- (length place)))
            with pos = (car (last place))
            for k being the hash-keys of (index self)
            for v being the hash-values of (index self)
            when (and (<= (length parent) (length v))
                      (every #'= parent v)
                      (<= pos (nth (length parent) v)))
            do (incf (nth (length parent) v)))))
  (:method ((self pipe) (elt array) place)
    (prog1
        (multiple-value-bind (parent pos) (find-parent-element self place)
          (insert-element* elt parent pos))
      (loop with parent = (subseq place 0 (1- (length place)))
            with pos = (car (last place))
            for k being the hash-keys of (index self)
            for v being the hash-values of (index self)
            when (and (<= (length parent) (length v))
                      (every #'= parent v)
                      (<= pos (nth (length parent) v)))
            do (incf (nth (length parent) v)))))
  (:method ((self pipe) (elt (eql :pipe)) place)
    (insert-element self (make-pipe) place))
  (:method ((self pipe) elt (place symbol))
    (if place
        (insert-element self elt (resolve-element self place))
        (call-next-method))))

(defgeneric replace-element (self place pipe)
  (:documentation "Replace a place with a pipe.
This happens simply through REMOVE-ELEMENT and INSERT-ELEMENT. PLACE can be a
name, as in FIND-ELEMENT.

Note that this will destroy names due to REMOVE-PLACE.

Returns the segment.")
  (:method ((self pipe) place (elt element))
    (remove-element self place)
    (insert-element* self elt place))
  (:method ((self pipe) place (elt array))
    (remove-element self place)
    (insert-element* self elt place))
  (:method ((self pipe) place (elt (eql :pipe)))
    (replace-element self place (make-pipe)))
  (:method ((self pipe) (place symbol) pipe)
    (if place
        (replace-element self (resolve-element self place) pipe)
        (call-next-method))))

(defgeneric set-element-id (pipe path name)
  (:method ((pipe pipe) (path list) (name symbol))
    (setf (gethash name (index pipe)) path)))

(defgeneric move-element (pipe elt new-elt)
  (:method ((pipe pipe) elt new-elt)
    (prog1
        (let ((e (remove-element pipe elt)))
          (insert-element* pipe e new-elt))
      (loop for k being the hash-keys of (index pipe)
            for v being the hash-values of (index pipe)
            when (and (<= (length elt) (length v))
                      (every #'= elt v))
            do (set-element-id pipe (append elt (subseq v (length elt))) k)))))

(defgeneric msg (elt msg)
  (:method ((elt pipe) msg)
    (msg (pipe elt) msg))
  (:method ((elt pipe) (msg string))
    (msg (pipe elt) (make-instance 'simple-message :content msg)))
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

(defclass simple-message (message)
  ((content :initarg :content
            :accessor message-content)))

(defclass condition-message (message)
  ((condition :initarg :condition
              :initform (required-argument "CONDITION")
              :accessor message-condition)))

;;; Macros
;; This is from Shinmera's VERBOSE
(defmacro defpipe ((pipeline &optional place) &body elements)
  "Make a new array of ELEMENTS and apply it to the PIPE slot of object
PIPE. Optional arg PLACE designates the position to insert the elements at
when the slot is already filled."
  (with-gensyms (pipe parent c)
    (let ((index (loop for i from 0
                       for e in elements
                       for id = (getf (rest e) :id)
                       when id collect (list i id))))
      `(let ((,parent ,pipeline)
             (,pipe (make-pipe)))
         ,@(loop for (ty . args) in elements
                 collect `(insert-element* (make-instance 
                                               ',ty
                                             ,@(remf args :id))
                                           ,pipe))
         
         (add-element ,parent ,pipe ,place)
         ,(when index
            `(let ((,c (1- (length (pipe ,parent)))))
               ,@(loop for (i id) in index
                       collect `(set-element-id ,parent (list ,c ,i) ,id))))
         ,parent))))

(defun defpipe* (parent &rest elements)
  (let ((index))
    (loop for i from 0
          for e in elements
          if (consp e)
          do (std/macs::when-let ((id (getf (cdr e) :id)))
               (push (cons i id) index)))
    (let ((pipe (make-pipe)))
      (loop for elt in elements
            do (insert-element*
                (typecase elt
                  (atom (make-instance elt))
                  (cons 
                   (remf (cdr elt) :id)
                   (apply 'make-instance elt)))
                pipe))
      (setf (pipe parent) pipe)
      (when index
        (let ((c (1- (length pipe))))
          (loop for (i . id) in index
                do (set-element-id parent (list c i) id))))
      parent)))
