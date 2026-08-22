;;; cache.lisp --- Cache Objects

;; Cache Object Protocol

;;; Commentary:

;; based on CACLE: https://github.com/jlahd/cacle

;; CACLE provides a CACHE class which obtains a block of data from a provider
;; function, given the block's key. The key is a user-defined value that is
;; used in an EQL hash-table, which provides storage for the cache itself.

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

;; Note: as of <2026-01-13 Tue> we've refactored to resemble the CACLE API as
;; closely as possible. Our initial implementation didn't cut it for XFT
;; caching for WM.

;;; Code:
(in-package :obj/cache)

;;; Cache Table
;; Simple cache - based on value-weak hash-tables.
(defun make-cache-table (&rest args)
  "Make a value-weak hashtable. When value gets collected so does the key."
  (apply 'make-hash-table :weakness :value args))

(defun get-cache (key cache)
  "Get a value from a cache-table."
  (let ((val (gethash key cache)))
    (if val (values (sb-ext:weak-pointer-value val) t)
        (values nil nil))))

(defun (setf get-cache) (value key cache)
  "Set a value in a cache-table."
  (let ((w (sb-ext:make-weak-pointer value)))
    (sb-ext:finalize value (lambda () (remhash key cache)))
    (setf (gethash key cache) w)
    value))

(defun map-cache (fn cache)
  (with-hash-table-iterator (nextfn cache)
    (loop  
       (multiple-value-bind (validp key value) (nextfn)
         (when (not validp)
           (return-from map-cache))
         (funcall fn key (sb-ext:weak-pointer-value value))))))

(defun dump-cache (cache)
  (mumble "Dumping cache: ~A~%" cache)
  (map-cache #'(lambda (k v) 
                 (format t ":k ~A :v ~A~%" k v))
             cache))

;;; Entries
(defclass cache-entry ()
  ((key :accessor key :initarg :key)
   (data :initarg :data :accessor data)
   (pending :initarg :pending)
   (busy :accessor entry-busy :initform 0)
   (size :reader entry-size)
   (deadline :reader deadline :initarg :deadline)))

(defmethod print-object ((obj cache-entry) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ ":key " stream)
    (prin1 (slot-value obj 'key) stream)))

(defmethod entry-valid-p ((entry cache-entry))
  (slot-boundp entry 'size))

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

(defclass indexed-cache-entry (cache-entry)
  ((idx :accessor idx)))

(defclass heap-cache-entry (indexed-cache-entry)
  ((weight :accessor weight :initform 0 :initarg :weight)))

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
    (setf (idx e1) i2
	  (idx e2) i1
	  (aref heap i1) e2
	  (aref heap i2) e1)
    (values e2 e1)))

(defun sink-down (heap idx &optional prefer-to-sink)
  (let ((me (aref heap idx))
	(left (heap-left heap idx))
	(right (heap-right heap idx)))
    (unless (and (or (null left)
		     (< (weight me)
			(weight left))
		     (and (not prefer-to-sink)
			  (= (weight me)
			     (weight left))))
		 (or (null right)
		     (< (weight me)
			(weight right))
		     (and (not prefer-to-sink)
			  (= (weight me)
			     (weight right)))))
      ;; heavier than (one of) children, do sink
      (let ((lightest (if (and right
			       (< (weight right)
				  (weight left)))
			  (heap-right-idx idx)
			  (heap-left-idx idx))))
	(heap-swap heap idx lightest)
	(sink-down heap lightest prefer-to-sink)))))

(defun bubble-up (heap idx)
  (let ((me (aref heap idx))
	(parent (heap-parent heap idx)))
    (unless (or (null parent)
		(>= (weight me)
		    (weight parent)))
      ;; lighter than parent, do bubble
      (let ((p (heap-parent-idx idx)))
	(heap-swap heap idx p)
	(bubble-up heap p)))))

;;; Policy
(defclass cache-policy ()
  ())

(defgeneric entry-added (policy entry))

(defgeneric access-entry (policy entry))

(defgeneric entry-removed (policy entry))

(defgeneric evict-entry (policy size))

(defclass linked-list-cache-policy (cache-policy)
  ((head :initform (make-instance 'linked-cache-entry) :reader linked-list-head)))

(defmethod print-object ((obj linked-list-cache-policy) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (loop with head = (slot-value obj 'head)
          for i = (slot-value head 'next) then (slot-value i 'next)
          until (eq i head)
          do (format stream " ~a" (slot-value i 'key)))))

(defmethod entry-added :before ((policy linked-list-cache-policy) (entry cache-entry))
  (change-class entry 'linked-cache-entry))

(defmethod access-entry ((policy linked-list-cache-policy) (entry linked-cache-entry))
  ;; No time-based validation
  t)

(defmethod entry-added ((policy linked-list-cache-policy) (entry cache-entry))
  ;; Push to head of the queue
  (link-after entry (slot-value policy 'head)))

(defmethod entry-removed ((policy linked-list-cache-policy) (entry linked-cache-entry))
  (unlink entry))

;; Generic array-storage policies
(defclass array-cache-policy (cache-policy)
  ((data :initform (make-array 16 :adjustable t :fill-pointer 0))
   (unused :initform (make-array 16 :adjustable t :fill-pointer 0))))

(defmethod entry-added :before ((policy array-cache-policy) (entry cache-entry))
  (change-class entry 'indexed-cache-entry))

(defmethod access-entry ((policy array-cache-policy) (entry cache-entry))
  t)

(defmethod entry-added ((policy array-cache-policy) (entry cache-entry))
  (with-slots (data unused) policy
    (if (zerop (length unused))
        (setf (idx entry) (vector-push-extend entry data))
        (let ((i (vector-pop unused)))
          (setf (idx entry) i
                (aref data i) entry)))))

(defmethod entry-removed ((policy array-cache-policy) (entry indexed-cache-entry))
  (with-slots (data unused) policy
    (let ((i (idx entry)))
      (vector-push-extend i unused)
      (setf (aref data i) nil
            (idx entry) nil)
      (when (> (length unused) (/ (length data) 4))
        (let ((w 0))
          (loop for i below (length data)
                for e = (aref data i)
                when e
                do (if (= w i)
                       (incf w)
                       (setf (idx e) w
                             (aref data w) e
                             w (1+ w))))
          (setf (fill-pointer data) w
                (fill-pointer unused) 0))))))

;; FIFO: discard oldest
(defclass fifo-cache-policy (linked-list-cache-policy)
  ())

(defmethod evict-entry ((policy fifo-cache-policy) size-hint)
  (declare (ignore size-hint))
  (with-slots (head) policy
    (let ((last (slot-value head 'prev)))
      (unless (eq last head)
        (unlink last)
        last))))

;; LIFO: discard newest
(defclass lifo-cache-policy (linked-list-cache-policy)
  ())

(defmethod evict-entry ((policy lifo-cache-policy) size-hint)
  (declare (ignore size-hint))
  (with-slots (head) policy
    (let ((first (slot-value head 'next)))
      (unless (eq first head)
        (unlink first)
        first))))

;; LRU: discard least recent
(defclass lru-cache-policy (fifo-cache-policy)
  ())

(defmethod access-entry ((policy lru-cache-policy) (entry cache-entry))
  (unlink entry)
  (link-after entry (slot-value policy 'head))
  t)

;; MRU: discard most recent
(defclass mru-cache-policy (lifo-cache-policy)
  ())

(defmethod access-entry ((policy mru-cache-policy) (entry cache-entry))
  (unlink entry)
  (link-after entry (slot-value policy 'head))
  t)

;; randomly discard
(defclass random-cache-policy (array-cache-policy)
  ())

(defmethod evict-entry ((policy random-cache-policy) size-hint)
  (declare (ignore size-hint))
  (with-slots (data unused) policy
    (when (> (- (length data) (length unused)) 0)
      (let ((e (loop for i = (random (length data))
                     for e = (aref data i)
                     while (null e)
                     finally (return e))))
        (entry-removed policy e)
        e))))

;; LFU: discard least frequent
(defclass lfu-cache-policy (cache-policy)
  ((heap :initform (make-array 16 :adjustable t :fill-pointer 0))))

(defmethod entry-added ((policy lfu-cache-policy) (entry cache-entry))
  (change-class entry 'heap-cache-entry)
  (with-slots (heap) policy
    (setf (weight entry) 1
          (idx entry) (vector-push-extend entry heap))
    (bubble-up heap (idx entry))))

(defmethod access-entry ((policy lfu-cache-policy) (entry heap-cache-entry))
  (incf (weight entry))
  (sink-down (slot-value policy 'heap) (idx entry) t)
  t)

(defmethod entry-removed ((policy lfu-cache-policy) (entry heap-cache-entry))
  (with-slots (heap) policy
    (let ((i (idx entry)))
      (setf (idx entry) nil)
      (unless (= i (1- (length heap)))
        (setf (aref heap i) (vector-pop heap)
              (idx (aref heap i)) i)
        (sink-down heap i)))))

(defmethod evict-entry ((policy lfu-cache-policy) size-hint)
  (declare (ignore size-hint))
  (with-slots (heap) policy
    (when (> (length heap) 0)
      (let* ((lightest (aref heap 0))
             (heaviest (vector-pop heap)))
        (when (> (length heap) 0)
          (setf (aref heap 0) heaviest
                (idx heaviest) 0)
          (sink-down heap 0 t))
        lightest))))

;; LFUDA: discard least frequent (with dynamic aging)
(defclass lfuda-cache-policy (lfu-cache-policy)
  ((age :initform 0)))

(defmethod entry-added ((policy lfuda-cache-policy) (entry cache-entry))
  (call-next-method)
  (incf (weight entry) (slot-value policy 'age))
  (sink-down (slot-value policy 'heap) (idx entry)))

(defmethod evict-entry ((policy lfuda-cache-policy) size-hint)
  (declare (ignore size-hint))
  (let ((target (call-next-method)))
    (when target
      (setf (slot-value policy 'age) (weight target)))
    target))

;;; Cache
(defclass cache ()
  ((max-size :initarg :max-size :reader cache-max-size)
   (size :initform 0)
   (lock :initform (make-mutex :name "cache"))
   (table :initarg :table :accessor table
          :documentation "A hash-table containing all cached entries.")
   (provider :initarg :provider :reader cache-provider
           :documentation "A function which provisions cache entries given a single argument - used
as the source of the cache.")
   (cleanup :initarg :cleanup :accessor cache-cleanup
            :documentation "A function which is called on the DATA slot of a given cache entry on flush.")
   (lifetime :initarg :lifetime :initform nil :reader cache-lifetime)
   (policy :initarg :policy :reader cache-policy :type cache-policy
           :documentation "The designated policy of this cache instance.")))


(defmethod print-object ((obj cache) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-mutex ((slot-value obj 'lock))
      (princ ":count " stream)
      (princ (hash-table-count (slot-value obj 'table)) stream)
      (princ " :size " stream)
      (princ (slot-value obj 'size) stream)
      (princ "/" stream)
      (prin1 (cache-max-size obj) stream))))

(defmethod initialize-instance ((cache cache) &key policy provider (test 'eql)
                                              &allow-other-keys)
  (call-next-method)
  (unless provider
    (error ":provider must be defined"))
  (setf (slot-value cache 'table) (make-hash-table :test test))
  (cond ((and policy (null (cache-max-size cache)))
         (error "Policy defined, but no maximum size"))
        ((null policy)
         (unless (null (cache-max-size cache))
           (error "Maximum size defined, but policy missing")))
        ((typep policy 'cache-policy)
         (setf (slot-value cache 'policy) policy))
        (t
         (error "Invalid policy ~s" policy))))

(defun make-cache (capacity provider &key (test 'eql) (policy :fifo) lifetime cleanup)
  "Create a new cache with the specified capacity, provider function, and options."
  (when (or (keywordp policy)
            (and (listp policy)
                 (keywordp (first policy))))
    (let ((base-type (if (keywordp policy) policy (first policy)))
          (args (if (keywordp policy) nil (rest policy))))
      (setf policy (apply #'make-instance
                          (ecase base-type
                            (:bélády (error "Clairvoyance hardware not installed"))
                            (:fifo 'fifo-cache-policy)
                            (:lifo 'lifo-cache-policy)
                            (:lru 'lru-cache-policy)
                            (:mru 'mru-cache-policy)
                            (:random 'random-cache-policy)
                            (:lfu 'lfu-cache-policy)
                            (:lfuda 'lfuda-cache-policy))
                          args))))
  (make-instance 'cache
                 :test test
                 :max-size capacity
                 :provider provider
                 :policy policy
                 :lifetime lifetime
                 :cleanup cleanup))

(defvar *cleanup-list*)
(defmacro with-collected-cleanups ((cache) &body body)
  (let ((i (gensym))
        (fn (gensym)))
    `(let* ((,fn (with-mutex ((slot-value ,cache 'lock))
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
        ((zerop (entry-busy entry))
         (remhash (key entry) hash)
         (push (slot-value entry 'data) *cleanup-list*))
        ((< (entry-busy entry) 0)
         (error "Internal error: double prepare-cleanup for ~s" entry))
        (t
         (setf (entry-busy entry) (- (entry-busy entry))))))

(defun ensure-cache-size (cache)
  (with-slots (policy table max-size size) cache
    (loop while (> size max-size)
          for old = (evict-entry policy (- max-size size))
          while old
          do (progn
               (decf size (slot-value old 'size))
               (prepare-cleanup old table)))))

(defmethod cache-size ((cache cache))
  "Returns the current size of the cache."
  (with-mutex ((slot-value cache 'lock))
    (slot-value cache 'size)))

(defmethod cache-count ((cache cache))
  "Returns the current count of items in the cache."
  (with-mutex ((slot-value cache 'lock))
    (hash-table-count (slot-value cache 'table))))

(defmethod set-cache-max-size ((cache cache) new-max)
  (with-slots (lock max-size policy) cache
    (with-collected-cleanups (cache)
      (with-mutex (lock)
        (setf max-size new-max)
        (when policy
          (ensure-cache-size cache))))))

(defsetf cache-max-size set-cache-max-size)

(defmethod set-cache-provider ((cache cache) new-provider)
  (with-slots (lock provider) cache
    (with-mutex (lock)
      (setf provider new-provider))))

(defsetf cache-provider set-cache-provider)

(defmethod set-cache-cleanup ((cache cache) new-cleanup)
  (with-slots (lock cleanup) cache
    (with-mutex (lock)
      (setf cleanup new-cleanup))))

(defsetf cache-cleanup set-cache-cleanup)

(defmethod set-cache-lifetime ((cache cache) new-lifetime)
  (with-slots (lock lifetime) cache
    (with-mutex (lock)
      (setf lifetime new-lifetime))))

(defsetf cache-lifetime set-cache-lifetime)

(defmethod cache-fetch ((cache cache) key &key only-if-cached force-fetch)
  "Fetch an item for the given key.
If the item is not currently in the cache, or has expired, it is fetched from the provider and stored in the cache.
If force-fetch is specified, a new value is fetched from the provider even if it already exists in the cache.
If a cleanup function is defined for the cache, remember to call cache-release with the second value returned by cache-fetch!"
  (with-slots (lock table policy provider) cache
    (with-collected-cleanups (cache)
      (multiple-value-bind (hit data entry)
          (with-mutex (lock)
            (when force-fetch
              (let ((entry (gethash key table)))
                (when entry
                  (prepare-cleanup entry table)
                  (decf (slot-value cache 'size) (slot-value entry 'size))
                  (when policy
                    (entry-removed policy entry)))))
            (flet ((miss ()
                     (let ((entry (make-instance 'cache-entry :key key :pending (sb-thread:make-waitqueue))))
                       (setf (gethash key table) entry)
                       (values nil entry))))
              (loop
                 (let ((entry (gethash key table)))
                   (cond ((and (null entry)
                               only-if-cached)
                          ;; cache miss, and no waiting
                          (return (values t nil nil)))

                         ((null entry)
                          ;; cache miss - initialize fetch from source
                          (return (miss)))

                         ((and (slot-boundp entry 'pending)
                               only-if-cached)
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
                                    (if (>= (entry-busy entry) 0)
                                        (incf (entry-busy entry))
                                        (decf (entry-busy entry)))
                                    (return (values t (slot-value entry 'data) entry)))
                                  (return (values t (slot-value entry 'data)))))))

                         ((and entry policy
                               (or (and (slot-boundp entry 'deadline)
                                        (<= (slot-value entry 'deadline)
                                            (get-universal-time)))
                                   (and (>= (entry-busy entry) 0)
                                        (not (access-entry policy entry)))))
                          ;; cached data has expired or been invalidated
                          (remhash key table)
                          (prepare-cleanup entry table)
                          (decf (slot-value cache 'size) (slot-value entry 'size))
                          (entry-removed policy entry)
                          (if only-if-cached
                              (return (values t nil nil)) ; no waiting
                              (return (miss))))

                         ((cache-cleanup cache)
                          (if (>= (entry-busy entry) 0)
                              (incf (entry-busy entry))
                              (decf (entry-busy entry)))
                          (return (values t (slot-value entry 'data) entry)))

                         (t
                          (return (values t (slot-value entry 'data) nil))))))))
        (if hit
            (values data entry)
            (multiple-value-bind (content size)
                (handler-case (funcall provider key)
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
                  (setf (slot-value data 'data) content
                        (slot-value data 'size) size)
                  (with-slots (lifetime) cache
                    (when lifetime
                      (setf (slot-value data 'deadline)
                            (+ (get-universal-time) lifetime))))
                  (condition-notify (slot-value data 'pending))
                  (slot-makunbound data 'pending)
                  (incf (slot-value cache 'size) size)
                  (when policy
                    (ensure-cache-size cache)
                    (entry-added policy data))
                  (if (cache-cleanup cache)
                      (progn
                        (incf (entry-busy data))
                        (values content data))
                      (values content nil))))))))))

(defmethod cache-release ((cache cache) entry)
  "Releases a reference for an item fetched earlier.
An item fetched from the cache with cache-fetch will not be cleaned up before it is released."
  (when entry
    (with-slots (lock table cleanup) cache
      (let ((to-clean (with-mutex (lock)
                        (let ((busy (entry-busy entry)))
                          (cond ((zerop busy)
                                 (error "Double release for item with the key ~a" (key entry)))
                                ((> busy 0)
                                 (decf (entry-busy entry))
                                 nil)
                                (t
                                 (when (zerop (incf (entry-busy entry)))
                                   (when (eq (gethash (key entry) table) entry)
                                     (remhash (key entry) table))
                                   (slot-value entry 'data))))))))
        (when (and cleanup to-clean)
          (funcall cleanup to-clean)))))
  nil)

(defmacro with-cache (var (cache key &key only-if-cached) &body body)
  "Combines a cache-fetch and cache-release in a form."
  (let ((c-var (gensym))
        (tag (gensym)))
    `(let ((,c-var ,cache))
       (multiple-value-bind (,var ,tag)
           (cache-fetch ,c-var ,key ,@(and only-if-cached '(:only-if-cached t)))
         (unwind-protect
              (progn ,@body)
           (cache-release ,c-var ,tag))))))

(defmethod cache-remove ((cache cache) key)
  "Remove the item with the specified key from the cache."
  (with-slots (lock table policy size) cache
    (with-collected-cleanups (cache)
      (with-mutex (lock)
        (let ((entry (gethash key table)))
          (when entry
            (prepare-cleanup entry table)
            (decf size (slot-value entry 'size))
            (when policy
              (entry-removed policy entry))
            t))))))

(defmethod cache-flush ((cache cache))
  "Flush the cache, removing all items currently stored in it. If a cleanup
function is defined for the cache, it is called for every item."
  (with-slots (lock table policy size cleanup) cache
    (with-collected-cleanups (cache)
      (with-mutex (lock)
        (maphash #'(lambda (k v)
                     (declare (ignore k))
                     (prepare-cleanup v table)
                     (entry-removed policy v))
                 table)
        (setf size 0)))
    nil))

(defmethod cache-sanity-check ((cache cache))
  (with-slots (lock table policy size) cache
    (with-mutex (lock)
      (let ((seen (make-hash-table :test 'eq)))
        (dolist (i (list-entries policy))
          (let ((v (gethash (key i) table)))
            (unless v
              (error "Cachen entry missing from hashtable: ~s" i))
            (unless (eq i v)
              (error "Cache entry mismatch: ~s in hashtable, ~s in policy" v i)))
          (setf (gethash i seen) t))
        (let ((total 0))
          (maphash #'(lambda (k v)
                       (declare (ignore k))
                       (when (>= (entry-busy v) 0)
                         (unless (gethash v seen)
                           (error "Cache entry missing from policy: ~s" v))
                         (incf total (entry-size v))))
                   table)
          (unless (= total size)
            (error "Cache size mismatch: cache reports ~a, sum of entries is ~a" size total))))))
  t)

(defun list-entries (policy)
  (etypecase policy
    (lfu-cache-policy (coerce (slot-value policy 'heap) 'list))
    (linked-list-cache-policy
     (loop with head = (slot-value policy 'head)
           for i = (slot-value head 'next) then (slot-value i 'next)
           until (eq i head)
           collect i))
    (array-cache-policy
     (loop for i across (slot-value policy 'data)
           when i collect i))))
