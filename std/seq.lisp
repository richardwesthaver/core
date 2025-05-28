;;; std/seq.lisp --- Standard Sequences

;;

;;; Code:
(in-package :std/seq)

(defun take (n seq)
  "Return, at most, the first N elements of SEQ, as a *new* sequence
of the same type as SEQ.

If N is longer than SEQ, SEQ is simply copied.

If N is negative, then |N| elements are taken (in their original
order) from the end of SEQ."
  (declare (type signed-array-length n))
  (sb-impl::seq-dispatch 
   seq
   (if (minusp n)
       (last seq (abs n))
       (firstn n seq))
   (if (minusp n)
       (subseq seq (max 0 (+ (length seq) n)))
       (subseq seq 0 (min n (length seq))))))

(defun starts-with-subseq (prefix sequence &rest args
                                           &key
                                           return-suffix
                                           &allow-other-keys)
  "Test whether the first elements of SEQUENCE are the same (as per TEST) as the elements of PREFIX.

If RETURN-SUFFIX is T the function returns, as a second value, a
sub-sequence or displaced array pointing to the sequence after PREFIX."
  (declare (dynamic-extent args))
  (let ((sequence-length (length sequence))
        (prefix-length (length prefix)))
    (when (< sequence-length prefix-length)
      (return-from starts-with-subseq (values nil nil)))
    (flet ((make-suffix (start)
             (when return-suffix
               (cond
                 ((not (arrayp sequence))
                  (if start
                      (subseq sequence start)
                      (subseq sequence 0 0)))
                 ((not start)
                  (make-array 0
                              :element-type (array-element-type sequence)
                              :adjustable nil))
                 (t
                  (make-array (- sequence-length start)
                              :element-type (array-element-type sequence)
                              :displaced-to sequence
                              :displaced-index-offset start
                              :adjustable nil))))))
      (remf args :return-suffix)
      (let ((mismatch (apply #'mismatch prefix sequence
                             args)))
        (cond
          ((not mismatch)
           (values t (make-suffix nil)))
          ((= mismatch prefix-length)
           (values t (make-suffix mismatch)))
          (t
           (values nil nil)))))))

(defun ends-with-subseq (suffix sequence &key (test #'eql))
  "Test whether SEQUENCE ends with SUFFIX. In other words: return true if
the last (length SUFFIX) elements of SEQUENCE are equal to SUFFIX."
  (let ((sequence-length (length sequence))
        (suffix-length (length suffix)))
    (when (< sequence-length suffix-length)
      ;; if SEQUENCE is shorter than SUFFIX, then SEQUENCE can't end with SUFFIX.
      (return-from ends-with-subseq nil))
    (loop for sequence-index from (- sequence-length suffix-length) below sequence-length
          for suffix-index from 0 below suffix-length
          when (not (funcall test (elt sequence sequence-index) (elt suffix suffix-index)))
          do (return-from ends-with-subseq nil)
          finally (return t))))

(defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position delimiter seq 
                                         :end right
                                         :from-end t
                                         other-keys)
                                  -1)
                              (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
        (loop for left = start then (+ right 1)
              for right = (min (or (apply #'position delimiter seq 
                                          :start left
                                          other-keys)
                                   len)
                               end)
              unless (and (= right left) 
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values subseqs left)
              else
              collect (subseq seq left right) into subseqs
              and sum 1 into nr-elts
              until (>= right end)
              finally (return (values subseqs right))))))

(defun split-sequence-if (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (when key-supplied 
                      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if predicate seq 
                                         :end right
                                         :from-end t
                                         other-keys)
                                  -1)
                              (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
        (loop for left = start then (+ right 1)
              for right = (min (or (apply #'position-if predicate seq 
                                          :start left
                                          other-keys)
                                   len)
                               end)
              unless (and (= right left) 
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values subseqs left)
              else
              collect (subseq seq left right) into subseqs
              and sum 1 into nr-elts
              until (>= right end)
              finally (return (values subseqs right))))))

(defun split-sequence-if-not (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
(CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (when key-supplied 
                      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if-not predicate seq 
                                         :end right
                                         :from-end t
                                         other-keys)
                                  -1)
                              (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
        (loop for left = start then (+ right 1)
              for right = (min (or (apply #'position-if-not predicate seq 
                                          :start left
                                          other-keys)
                                   len)
                               end)
              unless (and (= right left) 
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values subseqs left)
              else
              collect (subseq seq left right) into subseqs
              and sum 1 into nr-elts
              until (>= right end)
              finally (return (values subseqs right))))))

;; from hunchentoot
(defun starts-with-p (seq subseq &key (test 'eql))
  "Tests whether the sequence SEQ starts with the sequence
SUBSEQ.  Individual elements are compared with TEST."
  (let* ((length (length subseq))
         (mismatch (mismatch subseq seq
                             :test test)))
    (or (null mismatch)
        (<= length mismatch))))

;; from hunchentoot
(defun starts-with-one-of-p (seq subseq-list &key (test 'eql))
  "Tests whether the sequence SEQ starts with one of the
sequences in SUBSEQ-LIST.  Individual elements are compared with
TEST."
  (some (lambda (subseq)
          (starts-with-p seq subseq :test test))
        subseq-list))

;; matlisp
(definline copy-n (vec lst n)
  "Copy N elements of vector VEC into list LST."
  (declare (type vector vec)
           (type list lst)
           (type fixnum n))
  (loop :for i :of-type fixnum :from 0 :below n
        :for vlst := lst :then (cdr vlst)
        :do (setf (car vlst) (aref vec i)))
  lst)

;;; Queues
;;;; Conditions
(defun report-queue-size-limit-reached (condition stream)
  (let ((queue (error-queue condition))
        (element (error-element condition)))
    (format stream "Size limit (~D) reached for non-extensible ~
                    queue ~S while trying to enqueue element ~S onto it."
            (length (data queue)) queue element)))

(define-condition queue-size-limit-reached (error)
  ((queue :reader error-queue :initarg :queue)
   (element :reader error-element :initarg :element))
  (:report report-queue-size-limit-reached))
;;;; Basic Queue
(defstruct (basic-queue (:conc-name nil)
		        (:constructor %make-basic-queue (head tail)))
  (head (error "no head") :type list)
  (tail (error "no tail") :type list))

(defun make-basic-queue ()
  (%make-basic-queue nil nil))

(defun push-basic-queue (val queue)
  (declare (basic-queue queue))
  (let ((new (cons val nil)))
    (if (head queue)
	(setf (cdr (tail queue)) new
	      (head queue) new)
	(setf (tail queue) new))))

(defun pop-basic-queue (queue)
  (declare (basic-queue queue))
  (let ((node (head queue)))
    (if node
	(multiple-value-prog1 (values (car node) t)
	  (when (null (setf (head queue) (cdr node)))
	    (setf (tail queue) nil))
	  ;; clear node for conservative gcs
	  (setf (car node) nil
		(cdr node) nil))
	(values nil nil))))

(defun basic-queue-count (queue) (length (the list (head queue))))
(defun basic-queue-empty-p (queue) (not (head queue)))
(defun peek-basic-queue (queue) 
  (let ((node (head queue)))
    (values (car node)
	    (if node t nil))))

;;;; Raw Queue (vectorized)
(deftype raw-queue-count () 'std/type:array-length)

(defstruct (raw-queue (:constructor %make-raw-queue))
  (data (vector) :type simple-array)
  (start 0 :type std/type:array-index)
  (count 0 :type raw-queue-count))

(defmethod data ((self raw-queue))
  (raw-queue-data self))

(defun make-raw-queue (capacity)
  (%make-raw-queue :data (make-array capacity)))

(defun push-raw-queue (val queue)
  (declare (raw-queue queue))
  (with-slots (data start count) queue
    (setf (svref data (mod (+ start count) (length data))) val)
    (incf count))
  (values))

(defun pop-raw-queue (queue)
  (declare (raw-queue queue))
  (with-slots (data start count) queue
    (let ((data data))
      (if (plusp count)
          (multiple-value-prog1 (values (svref data start) t)
            (setf (svref data start) nil
                  start (mod (1+ start) (length data)))
            (decf count))
          (values nil nil)))))

(defun peek-raw-queue (queue)
  (declare (raw-queue queue))
  (with-slots (data start count) queue
    (if (plusp count)
        (values (svref data start) t)
        (values nil nil))))

(defun raw-queue-empty-p (queue) 
  (declare (raw-queue queue))
  (zerop (raw-queue-count queue)))

(defun raw-queue-full-p (queue) 
  (declare (raw-queue queue))
  (eql (raw-queue-count queue) (length (raw-queue-data queue))))

(defun raw-queue-capacity (queue) 
  (declare (raw-queue queue))
  (length (raw-queue-data queue)))

;;;; Vector Queue
(defstruct (vector-queue (:constructor %make-vector-queue))
  (impl (make-raw-queue 0) :type raw-queue)
  (lock (make-mutex))
  (%push nil)
  (%pop nil))

(defmethod data ((self vector-queue))
  (raw-queue-data (vector-queue-impl self)))

(defun make-vector-queue* (capacity)
  (%make-vector-queue :impl (make-raw-queue capacity)))

(defmacro with-vector-queue-lock (queue &body body)
  `(with-mutex ((vector-queue-lock ,queue))
     ,@body))

;; no lock
(declaim (inline push-vector-queue* pop-vector-queue*))
(defun push-vector-queue* (obj queue)
  (with-slots (impl lock %push %pop) queue
    (loop (cond ((< (raw-queue-count impl) (raw-queue-capacity impl))
		 (push-raw-queue obj impl)
		 (when %push
		   (condition-notify %push))
		 (return))
		(t
		 (condition-wait
		  (or %pop
		      (setf %pop (make-waitqueue)))
		  lock))))))

(defun push-vector-queue (obj queue)
  (declare (vector-queue queue))
  (with-mutex ((vector-queue-lock queue))
    (push-vector-queue* obj queue)
    (values)))

(defun pop-vector-queue* (queue)
  (declare (vector-queue queue))
  (with-slots (impl lock %push %pop) queue
    (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
	    (cond (presentp
		   (when %pop
		     (condition-notify %pop))
		   (return value))
		  (t 
		   (condition-wait
		    (or %push
			(setf %push (make-waitqueue)))
		    lock)))))))

(defun pop-vector-queue (queue)
  (declare (vector-queue queue))
  (with-mutex ((vector-queue-lock queue))
    (pop-vector-queue* queue)))

(defun %try-pop-vector-queue (queue timeout)
  ;; queue is empty and timeout is positive
  (with-countdown timeout
    (with-slots (impl lock %push %pop) queue
      (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
	      (when presentp
                (when %pop (condition-notify %pop))
                (return (values value t)))
	      (let ((time-remaining (time-remaining)))
		(when (or (not (plusp time-remaining))
			  (null (condition-wait
				 (or %push (setf %push (make-waitqueue)))
				 lock :timeout time-remaining)))
		  (return (values nil nil)))))))))

(defun %try-pop-vector-queue-with-timeout (queue timeout)
  (with-slots (impl) queue
    (if (basic-queue-empty-p impl)
	(%try-pop-vector-queue queue timeout)
	(pop-basic-queue impl))))

(defun try-pop-vector-queue* (queue)
  (with-slots (impl %pop) queue
    (multiple-value-bind (value presentp) (pop-raw-queue impl)
      (cond (presentp
             (when %pop (condition-notify %pop))
             (values value t))
            (t (values nil nil))))))

(defun try-pop-vector-queue (queue timeout)
  (if (raw-queue-empty-p (vector-queue-impl queue))
      (%try-pop-vector-queue-with-timeout queue timeout)
      (try-pop-vector-queue* queue)))

(macrolet ((define-queue-fn (name type raw)
	     `(progn
                (defun ,name (queue) 
		  (declare (,type queue))
		  (with-mutex ((vector-queue-lock queue))
		    (,raw (vector-queue-impl queue))))
                (defun ,(symbolicate (concatenate 'string (symbol-name name) "*")) (queue)
                  (declare (,type queue))
                  (,raw (vector-queue-impl queue))))))
  (define-queue-fn vector-queue-count vector-queue raw-queue-count)
  (define-queue-fn vector-queue-empty-p vector-queue raw-queue-empty-p)
  (define-queue-fn vector-queue-full-p vector-queue raw-queue-full-p)
  (define-queue-fn peek-vector-queue vector-queue peek-raw-queue))

(defun make-vector-queue (capacity &key initial-contents)
  (let ((queue (make-vector-queue* capacity)))
    (when initial-contents
      (block done
        (flet ((push-elem (elem)
                 (when (vector-queue-full-p queue)
                   (return-from done))
                 (push-vector-queue elem queue)))
	  (declare (dynamic-extent #'push-elem))
	  (map nil #'push-elem initial-contents))))
    queue))

;;;; Cons Queue
(defstruct (cons-queue (:constructor %make-cons-queue))
  (impl (make-basic-queue) :type basic-queue)
  (lock (sb-thread:make-mutex))
  (cvar nil))

(defmethod data ((self cons-queue))
  (cons-queue-impl self))

(defmacro with-cons-queue-lock (queue &body body)
  `(with-mutex ((cons-queue-lock ,queue))
     ,@body))

(declaim (inline push-vector-queue* pop-vector-queue*))

(defun push-cons-queue* (object queue) 
  (declare (cons-queue queue))
  (with-slots (impl cvar) queue
    (push-basic-queue object impl)
    (when cvar
      (condition-notify cvar)))
  (values))

(defun push-cons-queue (object queue) 
  (declare (cons-queue queue))
  (with-mutex ((cons-queue-lock queue))
    (push-cons-queue* object queue)))

(defun pop-cons-queue* (queue)
  (declare (cons-queue queue))
  (with-slots (impl lock cvar) queue
    (loop (multiple-value-bind (value presentp) (pop-basic-queue impl)
	    (if presentp
		(return value)
		(condition-wait (or cvar (setf cvar (make-waitqueue)))
				lock))))))

(defun pop-cons-queue (queue) 
  (declare (cons-queue queue))
  (with-mutex ((cons-queue-lock queue))
    (pop-cons-queue* queue)))

(defun %try-pop-cons-queue (queue timeout)
  ;; queue is empty and timeout is positive
  (with-countdown timeout
    (with-slots (impl lock cvar) queue
      (loop (multiple-value-bind (value presentp) (pop-basic-queue impl)
              (when presentp
                (return (values value t)))
              (let ((time-remaining (time-remaining)))
                (when (or (not (plusp time-remaining))
                          (null (condition-wait
                                 (or cvar (setf cvar (make-waitqueue)))
                                 lock :timeout time-remaining)))
                  (return (values nil nil)))))))))

(defun try-pop-cons-queue-with-timeout (queue timeout)
  (with-slots (impl) queue
    (if (basic-queue-empty-p impl)
        (%try-pop-cons-queue queue timeout)
        (pop-basic-queue impl))))

(defun try-pop-cons-queue (queue timeout)
  (with-slots (impl lock) queue
    (cond ((plusp timeout)
           (with-mutex (lock)
             (try-pop-cons-queue queue timeout)))
          (t
           ;; optimization: don't lock if nothing is there
           (with-mutex (lock :wait-p nil) 
             (when (not (basic-queue-empty-p impl))
               (return-from try-pop-cons-queue (pop-basic-queue impl))))
           (values nil nil)))))

(defun try-pop-cons-queue* (queue timeout)
  (if (plusp timeout)
      (try-pop-cons-queue-with-timeout queue timeout)
      (pop-basic-queue (cons-queue-impl queue))))

(macrolet ((define-queue-fn (name type raw)
             `(progn
                (defun ,name (queue) 
		  (declare (,type queue))
                  (with-mutex ((cons-queue-lock queue))
                    (,raw (cons-queue-impl queue))))
		(defun ,(symbolicate (concatenate 'string (symbol-name name) "*")) (queue)
		  (declare (,type queue))
		  (,raw (cons-queue-impl queue))))))
  (define-queue-fn cons-queue-count cons-queue basic-queue-count)
  (define-queue-fn cons-queue-empty-p cons-queue basic-queue-empty-p)
  (define-queue-fn peek-cons-queue cons-queue peek-basic-queue))

(defun make-cons-queue (&key initial-contents)
  (let ((queue (%make-cons-queue)))
    (when initial-contents
      (flet ((push-elem (elem)
               (push-cons-queue elem queue)))
        (declare (dynamic-extent #'push-elem))
        (map nil #'push-elem initial-contents)))
    queue))

;;; Priority Queue
;; this queue implementation is based on phoe's DAMN-FAST-PRIORITY-QUEUE
;; ref: https://github.com/phoe/damn-fast-priority-queue/blob/main/damn-fast-priority-queue/src.lisp

;; TODO 2025-05-27: make thread-safe version? currently not needed
(defvar *default-priority* 0)
(defvar *default-priority-queue-size* 256)
(deftype priority () '(unsigned-byte 32))
(deftype priority-vector () '(simple-array priority (*)))
(deftype priority-vector-extension () '(integer 2 256))

(defstruct (priority-queue (:constructor %make-priority-queue))
  (data (make-array *default-priority-queue-size*) :type simple-array)
  (priorities (make-array *default-priority-queue-size* :element-type 'priority) :type priority-vector)
  (size 0 :type array-length)
  (extension 256 :type priority-vector-extension)
  (extend-p t :type boolean))

(defmethod data ((self priority-queue))
  (priority-queue-data self))

(declaim (ftype (function (simple-array priority-vector array-length)
                          (values null &optional))
                heapify-upwards))
(definline heapify-upwards (data-vector prio-vector index)
  (declare (type simple-array data-vector))
  (declare (type priority-vector prio-vector))
  (declare (type array-length index))
  (do ((child-index index parent-index)
       (parent-index (ash (1- index) -1) (ash (1- parent-index) -1)))
      ((= child-index 0))
    (let ((child-priority (aref prio-vector child-index))
          (parent-priority (aref prio-vector parent-index)))
      (cond ((< child-priority parent-priority)
             (rotatef (aref prio-vector parent-index)
                      (aref prio-vector child-index))
             (rotatef (aref data-vector parent-index)
                      (aref data-vector child-index)))
            (t (return))))))

(declaim (ftype (function (queue t priority) (values null &optional)) push-priority-queue))
(definline push-priority-queue (queue object priority)
  (symbol-macrolet ((data-vector (priority-queue-data queue))
                    (prio-vector (priority-queue-priorities queue)))
    (let ((size (priority-queue-size queue))
          (extension-factor (priority-queue-extension queue))
          (length (array-total-size data-vector)))
      (when (>= size length)
        (unless (priority-queue-extend-p queue)
          (error 'queue-size-limit-reached :queue queue :element object))
        (let ((new-length (max 1 (mod (* length extension-factor)
                                      (ash 1 64)))))
          (declare (type array-length new-length))
          (when (<= new-length length)
            (error "Integer overflow while resizing array: new-length ~D is ~
                    smaller than old length ~D" new-length length))
          (setf data-vector (adjust-array data-vector new-length)
                prio-vector (adjust-array prio-vector new-length))))
      (setf (aref data-vector size) object
            (aref prio-vector size) priority)
      (heapify-upwards data-vector prio-vector (priority-queue-size queue))
      (incf (priority-queue-size queue))
      nil)))

(declaim (ftype (function (simple-array priority-vector array-index)
                          (values null &optional))
                heapify-downwards))
(definline heapify-downwards (data-vector prio-vector size)
  (declare (type simple-array data-vector))
  (declare (type priority-vector prio-vector))
  (let ((parent-index 0))
    (loop
      (let* ((left-index (+ (* parent-index 2) 1))
             (left-index-validp (< left-index size))
             (right-index (+ (* parent-index 2) 2))
             (right-index-validp (< right-index size)))
        (flet ((swap-left ()
                 (rotatef (aref prio-vector parent-index)
                          (aref prio-vector left-index))
                 (rotatef (aref data-vector parent-index)
                          (aref data-vector left-index))
                 (setf parent-index left-index))
               (swap-right ()
                 (rotatef (aref prio-vector parent-index)
                          (aref prio-vector right-index))
                 (rotatef (aref data-vector parent-index)
                          (aref data-vector right-index))
                 (setf parent-index right-index)))
          (declare (inline swap-left swap-right))
          (when (and (not left-index-validp)
                     (not right-index-validp))
            (return))
          (when (and left-index-validp
                     (< (aref prio-vector parent-index)
                        (aref prio-vector left-index))
                     (or (not right-index-validp)
                         (< (aref prio-vector parent-index)
                            (aref prio-vector right-index))))
            (return))
          (if (and right-index-validp
                   (<= (aref prio-vector right-index)
                       (aref prio-vector left-index)))
              (swap-right)
              (swap-left)))))))

(declaim (ftype (function (queue) (values t boolean &optional)) dequeue))
(definline pop-priority-queue (queue)
  (declare (type queue queue))
  (if (= 0 (priority-queue-size queue))
      (values nil nil)
      (let ((data-vector (priority-queue-data queue))
            (prio-vector (priority-queue-priorities queue)))
        (multiple-value-prog1 (values (aref data-vector 0) t)
          (decf (priority-queue-size queue))
          (let ((old-data (aref data-vector (priority-queue-size queue)))
                (old-prio (aref prio-vector (priority-queue-size queue))))
            (setf (aref data-vector 0) old-data
                  (aref prio-vector 0) old-prio))
          (heapify-downwards data-vector prio-vector (priority-queue-size queue))))))

(defun make-priority-queue (capacity &key initial-contents prioritize (element-type t))
  (let ((queue (%make-priority-queue
                :data (make-array capacity :element-type element-type)
                :priorities (make-array capacity :element-type 'priority))))
    (setf (priority-queue-size queue) capacity)
    (when initial-contents
      (flet ((push-elem (elem)
               (push-priority-queue elem queue (if prioritize (funcall prioritize elem) *default-priority*))))
        (declare (dynamic-extent #'push-elem))
        (map nil #'push-elem initial-contents)))
    queue))

;;;; Protocol
(deftype queue () '(or cons-queue vector-queue priority-queue))

(defun make-queue (&key capacity initial-contents prioritize)
  (cond 
    ((and capacity (not prioritize)) (make-vector-queue capacity :initial-contents initial-contents))
    ((not prioritize) (make-cons-queue :initial-contents initial-contents))
    (prioritize (make-priority-queue (or capacity *default-priority-queue-size*) :initial-contents initial-contents :prioritize prioritize))))
     
(defun call-with-cons-queue-lock (fn queue)
  (with-cons-queue-lock queue
    (funcall fn)))

(defun call-with-vector-queue-lock (fn queue)
  (with-vector-queue-lock queue
    (funcall fn)))

(defmacro with-queue-lock (queue &body body)
  `(call-with-queue-lock (lambda () ,@body) ,queue))

(defun cons-queue-full-p (queue) (declare (ignore queue)) nil)

(macrolet ((define-queue-fn (name params cons-name vector-name)
             `(defun ,name ,params
                (typecase ,(car (last params))
                  (cons-queue (,cons-name ,@params))
                  (vector-queue (,vector-name ,@params))
                  (t (error 'type-error
                            :datum ,(car (last params))
                            :expected-type 'queue)))))
           (define-try-pop-queue (name cons-name vector-name)
             `(defun ,name (queue &key timeout)
                (unless timeout
                  (setf timeout 0))
                (typecase queue
                  (cons-queue (,cons-name queue timeout))
                  (vector-queue (,vector-name queue timeout))
                  (t (error 'type-error
		            :datum queue
		            :expected-type 'queue))))))
  (define-queue-fn push-queue (obj queue)
    push-cons-queue
    push-vector-queue)
  (define-queue-fn push-queue* (obj queue)
    push-cons-queue*
    push-vector-queue*)
  (define-queue-fn pop-queue (queue)
    pop-cons-queue
    pop-vector-queue)
  (define-queue-fn pop-queue* (queue)
    pop-cons-queue*
    pop-vector-queue*)
  (define-queue-fn peek-queue (queue)
    peek-cons-queue
    peek-vector-queue)
  (define-queue-fn peek-queue* (queue)
    peek-cons-queue*
    peek-vector-queue*)
  (define-queue-fn queue-count (queue)
    cons-queue-count
    vector-queue-count)
  (define-queue-fn queue-count* (queue)
    cons-queue-count*
    vector-queue-count*)
  (define-queue-fn queue-empty-p (queue)
    cons-queue-empty-p
    vector-queue-empty-p)
  (define-queue-fn queue-empty-p* (queue)
    cons-queue-empty-p*
    vector-queue-empty-p*)
  (define-queue-fn queue-full-p (queue)
    cons-queue-full-p
    vector-queue-full-p)
  (define-queue-fn queue-full-p* (queue)
    cons-queue-full-p
    vector-queue-full-p*)

  (define-try-pop-queue try-pop-queue
    try-pop-cons-queue
    try-pop-vector-queue)
  (define-try-pop-queue try-pop-queue*
    try-pop-cons-queue*
    %try-pop-vector-queue)

  (define-queue-fn call-with-queue-lock (fn queue)
    call-with-cons-queue-lock
    call-with-vector-queue-lock))

;;; Accumulator

;; originally part of q/query, may serve useful in other contexts.

(defclass accumulator ()
  ((value :initarg :value :accessor accumulator-value)))

(defgeneric accumulate (self val)
  (:method ((self accumulator) val)
    (when val
      (setf (accumulator-value self) (+ val (accumulator-value self)))))
  (:method ((self list) val)
    (push val self)))

(defgeneric make-accumulator (self))

;; max-accumulator
(defclass max-accumulator (accumulator) ())

(defmethod accumulate ((self max-accumulator) (val number))
  (when (> val (accumulator-value self))
    (setf (accumulator-value self) val)))

;;; Iterator
#|

The iterator protocol allows subsequently accessing some or all elements of a
sequence in forward or reverse direction. Users first call
make-sequence-iterator to create an iteration state and receive functions to
query and mutate it. These functions allow, among other things, moving to,
retrieving or modifying elements of the sequence. An iteration state consists
of a state object, a limit object, a from-end indicator and the following six
functions to query or mutate this state: step endp element (setf element) index copy

See also: make-sequence-iterator with-sequence-iterator with-sequence-iterator-functions

|#

(defclass iterator ()
  ()
  (:documentation "Iterator superclass inherited by objects implementing the iterator protocol."))

;; Protocol
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
