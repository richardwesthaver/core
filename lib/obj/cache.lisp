;;; cache.lisp --- Cache Objects

;; Cache Object Protocol

;;; Commentary:

;; based on CACLE: https://github.com/jlahd/cacle

;; CACLE provides a similar object, CACHE which obtains a block of data from a
;; provider function, given the block's key. The key is a user-defined value
;; that is used in an EQL hash-table, which provides storage for the cache
;; itself.

#| Cache Replacement
- First In First Out (:fifo): Data that has been in the cache for the longest time is discarded
- Last In First Out (:lifo): Most recently added data is discarded
- Least Recently Used (:lru): Data that has gone unused for the longest time is discarded
- Most Recently Used (:mru): Most recently used data is discarded
- Random (:random): A randomly selected piece of data is discarded
- Least Frequently Used (:lfu): Data with the lowest number of fetches is discarded
- Least Frequently Used with Dynamic Aging (:lfuda): An aging variable is
  introduced to LFU to prefer discarding data that has been used a lot in the
  history but less often recently.
|#

;;; Code:
(in-package :obj/cache)

;;; Entry
(defclass cache-entry ()
  ((key :accessor key)
   (data :accessor data)
   (expiry :reader entry-expiry)))

(defclass indexed-cache-entry (cache-entry)
  ((index :accessor index)))

(defclass linked-cache-entry (cache-entry)
  ((next :reader next)
   (prev :reader prev)))

(defmethod shared-initialize ((entry linked-cache-entry) slot-names &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (when (or (eq slot-names t)
	    (find 'next slot-names))
    (setf (slot-value entry 'next) entry))
  (when (or (eq slot-names t)
	    (find 'prev slot-names))
    (setf (slot-value entry 'prev) entry)))

(defmethod unlink ((entry linked-cache-entry))
  (let ((n (slot-value entry 'next))
	(p (slot-value entry 'prev)))
    (when (and (eq n entry)
	       (eq p entry))
      (error "Attempt to unlink an already unlinked entry ~s" entry))
    (setf (slot-value n 'prev) p
	  (slot-value p 'next) n
	  (slot-value entry 'next) entry
	  (slot-value entry 'prev) entry)
    entry))

(defun ensure-unlinked (entry)
  (with-slots (next prev)
      entry
    (unless (and (eq next entry)
		 (eq prev entry))
      (error "Attempt to link an already linked entry ~s" entry))))

(defmethod link-before ((entry linked-cache-entry) (ref linked-cache-entry))
  (ensure-unlinked entry)
  (let ((n ref)
	(p (slot-value ref 'prev)))
    (setf (slot-value p 'next) entry
	  (slot-value n 'prev) entry
	  (slot-value entry 'next) n
	  (slot-value entry 'prev) p)
    entry))

(defmethod link-after ((entry linked-cache-entry) (ref linked-cache-entry))
  (ensure-unlinked entry)
  (let ((n (slot-value ref 'next))
	(p ref))
    (setf (slot-value p 'next) entry
	  (slot-value n 'prev) entry
	  (slot-value entry 'next) n
	  (slot-value entry 'prev) p)
    entry))

;;; Policy
(deftype cache-policy () 'keyword)

(defgeneric entry-added (policy queue entry)
  (:method (policy (queue cons-queue) (entry cache-entry))
    (change-class entry 'linked-cache-entry))
  (:method (policy (queue cons-queue) (entry cache-entry))
    (link-after entry (next queue)))
  (:method :before (policy (queue vector-queue) (entry cache-entry))
    (change-class entry 'indexed-cache-entry))
  (:method (policy (queue vector-queue) (entry cache-entry))
    (setf (index entry) (push-queue entry queue))))
          
(defgeneric access-entry (policy queue entry)
  (:method (policy (queue cons-queue) (entry cache-entry)) t)
  (:method (policy (queue vector-queue) (entry cache-entry)) t)
  (:method ((policy (eql :lru)) queue (entry cache-entry))
    (unlink entry)
    (link-after entry (next queue))
    t)
  (:method ((policy (eql :mru)) queue (entry cache-entry))
    (unlink entry)
    (link-after entry (next queue))
    t))

(defgeneric entry-removed (policy queue entry)
  (:method (policy (queue cons-queue) (entry cache-entry))
    (unlink entry))
  (:method (policy (queue vector-queue) (entry cache-entry))
    (let ((i (index entry)))
      (setf (index entry) nil
            (aref (data queue) i) nil)
      (let ((w 0))
	(loop for i below (queue-count queue)
	      for e = (aref (data queue) i)
	      when e
	      do (if (= w i)
		     (incf w)
		     (setf (index e) w
			   (aref (data queue) w) e
			   w (1+ w))))
	  (setf (fill-pointer (data queue)) w)))))

(defgeneric evict-entry (policy queue)
  (:method ((policy (eql :fifo)) queue)
    (let* ((next (next queue))
           (last (last next)))
      (unless (eq last next)
        (unlink last)
        last)))
  (:method ((policy (eql :lifo)) queue)
    (let* ((next (next queue))
           (first (next next)))
      (unless (eq first next)
        (unlink first)
        first)))
  (:method ((policy (eql :random)) queue)
    (unless (queue-full-p queue)
      (let ((e (loop for i = (random (raw-queue-capacity (queue queue)))
                     for e = (aref (data queue) i)
                     while (null e)
                     finally (return e))))
        (entry-removed policy queue e)
        e))))

;;; Cache
(defclass cache ()
  ((policy :initarg :policy :accessor cache-policy)
   (kernel :initarg :kernel :accessor kernel)
   (table :initarg :table :accessor table)
   (queue :initarg :queue :accessor queue)
   (lock :initarg :lock :accessor lock)))
