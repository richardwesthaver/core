;;; cache.lisp --- Cache Objects

;; Cache Object Protocol

;;; Commentary:

;; based on CACLE: https://github.com/jlahd/cacle

;; CACLE provides a similar object, CACHE which obtains a block of data from a
;; provider function, given the block's key. The key is a user-defined value
;; that is used in an EQL hash-table, which provides storage for the cache
;; itself.

;; CACLE supports a variety of replacement policies which we also support. The
;; only major interface change is that we use keywords to indicate the policy
;; instead of objects and the :LFUDA policy is inferred simply from fixnums.

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

;;; Cache Table
(defun make-cache-table (&rest args)
  "Make a value-weak hashtable. When value gets collected so does the key."
  (apply 'make-hash-table :weakness :value args))

(defun get-cache (key cache)
  "Get a value from a cache-table."
  (let ((val (gethash key cache)))
    (if val (values (sb-ext:weak-pointer-value val) t)
        (values nil nil))))

(defsetf get-cache setf-cache)

(defun setf-cache (key cache value)
  "Set a value in a cache-table."
  (let ((w (sb-ext:make-weak-pointer value)))
    (sb-ext:finalize value (make-finalizer key cache))
    (setf (gethash key cache) w)
    value))

(defun make-finalizer (key cache)
  (declare (ignorable key cache))
  (lambda () (remhash key cache)))

(defun remcache (key cache)
  (remhash key cache))

(defun map-cache (fn cache)
  (with-hash-table-iterator (nextfn cache)
    (loop  
       (multiple-value-bind (valid? key value) (nextfn)
         (when (not valid?)
           (return-from map-cache))
         (funcall fn key (sb-ext:weak-pointer-value value))))))

(defun dump-cache (cache)
  (format t "Dumping cache: ~A~%" cache)
  (map-cache #'(lambda (k v) 
                 (format t ":k ~A :v ~A~%" k v))
             cache))

;;; Entry
(defclass cache-entry ()
  ((key :accessor key :initarg :key)
   (data :accessor data)
   (pending :initarg :pending)
   (rc :accessor entry-rc :initform 0)
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

(defclass heap-cache-entry (indexed-cache-entry)
  ((weight :accessor entry-weight :initform 0 :initarg :weight)))

(defun heap-parent-idx (idx)
  (floor (1- idx) 2))

(defun heap-left-idx (idx)
  (1+ (* idx 2)))

(defun heap-right-idx (idx)
  (* (1+ idx) 2))

(defun heap-parent (heap idx)
  (and (> idx 0)
       (aref heap (heap-parent-idx idx))))

(defun heap-left (heap idx)
  (let ((left (heap-left-idx idx)))
    (and (< left (length heap))
	 (aref heap left))))

(defun heap-right (heap idx)
  (let ((right (heap-right-idx idx)))
    (and (< right (length heap))
	 (aref heap right))))

(defun heap-swap (heap i1 i2)
  (let ((e1 (aref heap i1))
	(e2 (aref heap i2)))
    (setf (index e1) i2
	  (index e2) i1
	  (aref heap i1) e2
	  (aref heap i2) e1)
    (values e2 e1)))

(defun sink-down (heap idx &optional prefer-to-sink)
  (let ((me (aref heap idx))
	(left (heap-left heap idx))
	(right (heap-right heap idx)))
    (unless (and (or (null left)
		     (< (entry-weight me)
			(entry-weight left))
		     (and (not prefer-to-sink)
			  (= (entry-weight me)
			     (entry-weight left))))
		 (or (null right)
		     (< (entry-weight me)
			(entry-weight right))
		     (and (not prefer-to-sink)
			  (= (entry-weight me)
			     (entry-weight right)))))
      ;; heavier than (one of) children, do sink
      (let ((lightest (if (and right
			       (< (entry-weight right)
				  (entry-weight left)))
			  (heap-right-idx idx)
			  (heap-left-idx idx))))
	(heap-swap heap idx lightest)
	(sink-down heap lightest prefer-to-sink)))))

(defun bubble-up (heap idx)
  (let ((me (aref heap idx))
	(parent (heap-parent heap idx)))
    (unless (or (null parent)
		(>= (entry-weight me)
		    (entry-weight parent)))
      ;; lighter than parent, do bubble
      (let ((p (heap-parent-idx idx)))
	(heap-swap heap idx p)
	(bubble-up heap p)))))

;;; Policy
(deftype cache-policy () '(or keyword fixnum))

(defgeneric entry-added (policy queue entry)
  (:method (policy (queue cons-queue) (entry cache-entry))
    (change-class entry 'linked-cache-entry))
  (:method (policy (queue cons-queue) (entry cache-entry))
    (link-after entry (next queue)))
  (:method :before (policy (queue vector-queue) (entry cache-entry))
    (change-class entry 'indexed-cache-entry))
  (:method (policy (queue vector-queue) (entry cache-entry))
    (setf (index entry) (push-queue* entry queue)))
  (:method ((policy (eql :lfu)) (queue vector-queue) (entry cache-entry))
    (change-class entry 'heap-cache-entry)
    (setf (entry-weight entry) 1
          (index entry) (push-queue* entry queue))
    (bubble-up (data queue) (index entry)))
  (:method ((policy fixnum) queue (entry cache-entry))
    (entry-added :lfu queue entry)
    (incf (entry-weight entry) policy)
    (sink-down (data queue) (index entry))))

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
    t)
  (:method ((policy (eql :lfu)) (queue vector-queue) (entry heap-cache-entry))
    (incf (entry-weight entry))
    (sink-down (data queue) (index entry) t)
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
	(setf (fill-pointer (data queue)) w))))
  (:method ((policy (eql :lfu)) (queue vector-queue) (entry heap-cache-entry))
    (let ((i (index entry)))
      (setf (index entry) nil)
      (unless (= i (1- (queue-count* queue)))
        (setf (aref (data queue) i) (pop-queue* queue)
              (index (aref (data queue) i)) i)
        (sink-down (data queue) i)))))

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
        e)))
  (:method ((policy (eql :lfu)) (queue vector-queue))
    (unless (queue-empty-p* queue)
      (let ((light (aref (data queue) 0))
            (heavy (pop-queue* queue)))
        (unless (queue-empty-p* queue)
          (setf (aref (data queue) 0) heavy
                (index heavy) 0)
          (sink-down (data queue) 0 t))
        light)))
  (:method ((policy fixnum) (queue vector-queue))
    (when-let ((target (evict-entry :lfu queue)))
      ;; CACLE updates the policy object here, we return the weight
      ;; (entry-weight target)
      target)))

;;; Cache
(defclass cache ()
  ((policy :initarg :policy :accessor cache-policy)
   (kernel :initarg :kernel :accessor kernel)
   (cleanup :initarg :cleanup :accessor cache-cleanup)
   (table :initarg :table :accessor table)
   (queue :initform (make-queue) :initarg :queue :accessor queue)))

(defmethod initialize-instance ((cache cache) &key policy kernel (test 'eql) capacity element-type
                                              &allow-other-keys)
  (call-next-method)
  (unless kernel (required-argument :kernel))
  (setf (slot-value cache 'table) (make-hash-table :test test)
        (slot-value cache 'queue) (make-queue :capacity capacity :element-type element-type))
  (cond ((and policy (not (typep (queue cache) 'vector-queue)))
	 (error "Policy defined, but queue is possibly infinite"))
	((null policy)
	 (unless (not (typep (queue cache) 'vector-queue))
	   (error "Queue size is defined, but policy missing")))
	((typep policy '(or keyword fixnum null))
	 (setf (slot-value cache 'policy) policy))
	(t
	 (error "Invalid policy ~s" policy))))

(defun make-cache (capacity provider &key (test 'eql) (policy :fifo) cleanup (element-type 'cache-entry))
  "Create a new cache with the specified capacity, kernel function, and options."
  (make-instance 'cache
    :test test
    :capacity capacity
    :kernel provider
    :policy policy
    :cleanup cleanup
    :element-type element-type))

(defvar *cleanup-list*)
(defmacro with-collected-cleanups ((cache) &body body)
  (with-gensyms (i fn)
    `(let* ((,fn (with-queue-lock (queue ,cache)
		   (slot-value ,cache 'cleanup)))
	    (*cleanup-list* (null ,fn)))
       (unwind-protect
	    (progn ,@body)
	 (when ,fn
	   (dolist (,i *cleanup-list*)
	     (funcall ,fn ,i)))))))

(defun prepare-cleanup (entry hash)
  (cond ((eq *cleanup-list* t)
	 (remhash (key entry) hash))
	((zerop (entry-rc entry))
	 (remhash (key entry) hash)
	 (push (slot-value entry 'data) *cleanup-list*))
	((< (entry-rc entry) 0)
	 (error "Internal error: double prepare-cleanup for ~s" entry))
	(t
	 (setf (entry-rc entry) (- (entry-rc entry))))))

;; REVIEW 2025-07-04: 
(defun ensure-cache-size (cache)
  (with-slots (policy table) cache
      (loop while (not (queue-full-p* (queue cache)))
	    for old = (evict-entry policy (queue cache))
	    while old
	    do (progn
	         ;; (decf size (slot-value old 'size))
	         (prepare-cleanup old table)))))

(defun cache-count (cache)
  "Returns the current count of items in the cache."
  (with-queue-lock (queue cache)
    (hash-table-count (slot-value cache 'table))))

(defmethod get-val ((cache cache) key &key shallow force)
  "Return the value associated with KEY in CACHE.

If the item is not currently in the cache, or has expired, it is fetched from
the provider and stored in the cache.

If FORCE is specified, a new value is fetched from the provider even if
it already exists in the cache.

If a cleanup function is defined for the cache, remember to call cache-release
with the second value returned by GET-VAL."
  (with-slots (table policy kernel) cache
    (let ((lock (lock (queue cache))))
      (with-collected-cleanups (cache)
        (multiple-value-bind (hit data entry)
	    (with-mutex (lock)
	      (when force
	        (let ((entry (gethash key table)))
		  (when entry
		    (prepare-cleanup entry table)
		    ;; (decf (cache-size cache) (slot-value entry 'size))
		    (when policy
		      (entry-removed policy (queue cache) entry)))))
	      (flet ((miss ()
		       (let ((entry (make-instance 'cache-entry :key key :pending (make-waitqueue))))
		         (setf (gethash key table) entry)
		         (values nil entry))))
	        (loop
		  (let ((entry (gethash key table)))
		    (cond ((and (null entry)
			        shallow)
			   ;; cache miss, and no waiting
			   (return (values t nil nil)))
			  ((null entry)
			   ;; cache miss - initialize fetch from source
			   (return (miss)))
			  ((and (slot-boundp entry 'pending)
			        shallow)
			   ;; cache hit - but data not yet ready, and no waiting
			   (return (values t nil nil)))
			  ((slot-boundp entry 'pending)
			   ;; cache hit - but data not yet ready
			   (let ((pending (slot-value entry 'pending)))
			     (condition-wait pending lock)
			     ;; note: the pending slot is no longer bound after the wait
			     (condition-notify pending)
			     ;; data now available
			     (when (eq (gethash key table) entry)
			       ;; ... and not immediately cleaned up
			       (if (cache-cleanup cache)
				   (progn
				     (if (>= (entry-rc entry) 0)
					 (incf (entry-rc entry))
					 (decf (entry-rc entry)))
				     (return (values t (slot-value entry 'data) entry)))
				   (return (values t (slot-value entry 'data)))))))
			  ((and entry policy
			        (or (and (slot-boundp entry 'expiry)
					 (<= (slot-value entry 'expiry)
					     (get-universal-time)))
				    (and (>= (entry-rc entry) 0)
					 (not (access-entry policy (queue cache) entry)))))
			   ;; cached data has expired or been invalidated
			   (remhash key table)
			   (prepare-cleanup entry table)
			   (decf (slot-value cache 'size) (slot-value entry 'size))
			   (entry-removed policy (queue cache) entry)
			   (if shallow
			       (return (values t nil nil)) ; no waiting
			       (return (miss))))

			  ((cache-cleanup cache)
			   (if (>= (entry-rc entry) 0)
			       (incf (entry-rc entry))
			       (decf (entry-rc entry)))
			   (return (values t (slot-value entry 'data) entry)))

			  (t
			   (return (values t (slot-value entry 'data) nil))))))))
	  (if hit
	      (values data entry)
	      (multiple-value-bind (content size)
		  (handler-case (funcall kernel key)
		    (error (e)
		      (with-mutex (lock)
		        (remhash key table)
		        (condition-notify (slot-value data 'pending))
		        (slot-makunbound data 'pending))
		      (error e)))
	        (with-collected-cleanups (cache)
		  (unless (typep size 'real)
		    (setf size (if content 1 0))
		    (warn "Cache provider did not return a proper size for the data - assuming size of ~d" size))
		  (with-mutex (lock)
		    (setf (slot-value data 'data) content)
			  ;; (slot-value data 'size) size)
		    ;; (with-slots (lifetime) cache
		    ;;   (when lifetime
		    ;;     (setf (slot-value data 'expiry)
		    ;;           (+ (get-universal-time) lifetime))))
		    (condition-notify (slot-value data 'pending))
		    (slot-makunbound data 'pending)
		    ;; (incf (slot-value cache 'size) size)
		    (when policy
		      (ensure-cache-size cache)
		      (entry-added policy (queue cache) data))
		    (if (cache-cleanup cache)
		        (progn
			  (incf (entry-rc data))
			  (values content data))
		        (values content nil)))))))))))

(defmethod cache-release ((cache cache) entry)
  "Releases a reference for an item fetched earlier.

An item fetched from the cache with cache-fetch will not be cleaned up before
it is released."
  (when entry
    (with-slots (table cleanup) cache
      (let ((to-clean 
              (with-queue-lock (queue cache)
		(let ((busy (entry-rc entry)))
		  (cond ((zerop busy)
			 (error "Double release for item with the key ~a" (key entry)))
			((> busy 0)
			 (decf (entry-rc entry))
			 nil)
			(t
			 (when (zerop (incf (entry-rc entry)))
			   (when (eq (gethash (key entry) table) entry)
			     (remhash (key entry) table))
			   (slot-value entry 'data))))))))
	(when (and cleanup to-clean)
	  (funcall cleanup to-clean)))))
  nil)

(defmacro with-cache (var (cache key &key shallow) &body body)
  "Combines a cache-fetch and cache-release in a form."
  (with-gensyms (c-var tag)
    `(let ((,c-var ,cache))
       (multiple-value-bind (,var ,tag)
	   (cache-fetch ,c-var ,key ,@(and shallow '(:shallow t)))
	 (unwind-protect
	      (progn ,@body)
	   (cache-release ,c-var ,tag))))))

(defmethod cache-remove ((cache cache) key)
  "Remove the item with the specified key from the cache."
  (with-slots (table policy) cache
    (with-collected-cleanups (cache)
      (with-queue-lock (queue cache)
	(let ((entry (gethash key table)))
	  (when entry
	    (prepare-cleanup entry table)
	    ;; (decf size (slot-value entry 'size))
	    (when policy
	      (entry-removed policy (queue cache) entry))
	    t))))))

(defmethod cache-flush ((cache cache))
  "Flush the cache, removing all items currently stored in it. If a cleanup
function is defined for the cache, it is called for every item."
  (with-slots (table policy cleanup) cache
    (with-collected-cleanups (cache)
      (with-queue-lock (queue cache)
	(maphash #'(lambda (k v)
		     (declare (ignore k))
		     (prepare-cleanup v table)
		     (entry-removed policy (queue cache) v))
		 table)
	;; (setf size 0)
        ))
    nil))
