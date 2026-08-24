;;; std/seq.lisp --- Standard Sequences

;;

;;; Code:
(in-package :std/seq)

(definline sequencep (object)
  (typecase object
    (sequence t)
    (t nil)))

(eval-always
  (definline unsplice (form)
    (if form (list form) nil))

  (defun item-predicate (item test test-not)
    (when (and test test-not)
      (error "Both :TEST and :TEST-NOT options given."))
    (when test-not
      (setf test (complement (ensure-function test-not)))
      (setf test-not nil))
    (if test
        (let ((test (ensure-function test)))
          (lambda (x)
            (funcall test item x)))
        (typecase item
          (integer
           (lambda (x)
             (declare (optimize (speed 3) (safety 0)) (integer x) (integer item))
             (= item x)))
          (character
           (lambda (x)
             (declare (optimize (speed 3) (safety 0)) (character x) (character item))
             (char= item x)))
          (otherwise
           (lambda (x)
             (declare (optimize (speed 3) (safety 0)))
             (eq item x)))))))

(defmacro dosequence ((var sequence &optional return) &body body)
  (with-gensyms (body-fn)
    `(block nil
       (flet ((,body-fn (,var) ,@body))
         (declare (dynamic-extent #',body-fn))
         (map nil #',body-fn ,sequence)
         ,@(unsplice (when return
                       `(let ((,var nil))
                          (declare (ignorable ,var))
                          ,return)))))))

(defmacro nth-value-or (nth-value &body forms)
  "Evaluates FORM arguments one at a time, until the NTH-VALUE returned by one
of the forms is true. It then returns all the values returned by evaluating
that form. If none of the forms return a true nth value, this form returns
NIL."
  (once-only (nth-value)
    (with-gensyms (values)
      `(let ((,values (multiple-value-list ,(first forms))))
         (if (nth ,nth-value ,values)
             (values-list ,values)
             ,(if (rest forms)
                  `(nth-value-or ,nth-value ,@(rest forms))
                  nil))))))

(defun starts-with (object sequence &key (test #'eql) (key #'identity))
  "Returns true if SEQUENCE is a sequence whose first element is EQL to OBJECT.
Returns NIL if the SEQUENCE is not a sequence or is an empty sequence."
  (let ((first-elt (typecase sequence
                     (cons (car sequence))
                     (sequence
                      (if (sequence:emptyp sequence)
                          (return-from starts-with nil)
                          (elt sequence 0)))
                     (t
                      (return-from starts-with nil)))))
    (funcall test (funcall key first-elt) object)))

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

(defun take* (n list)
  "Returns a list with the first n elements of the given list, and the
remaining tail of the list as a second value."
  (loop for l on list
        repeat n
        collect (car l) into result
        finally (return (values result l))))

(defun starts-with-subseq (prefix sequence 
                           &rest args
                           &key return-suffix &allow-other-keys)
  "Test whether the first elements of SEQUENCE are the same (as per TEST) as the
elements of PREFIX.

If RETURN-SUFFIX is T the function returns, as a second value, a sub-sequence
or displaced array pointing to the sequence after PREFIX."
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
                  (make-array 0 :element-type (array-element-type sequence)
                                :adjustable nil))
                 (t (make-array (- sequence-length start)
                                :element-type (array-element-type sequence)
                                :displaced-to sequence
                                :displaced-index-offset start
                                :adjustable nil))))))
      (remf args :return-suffix)
      (let ((mismatch (apply #'mismatch prefix sequence args)))
        (cond ((not mismatch) (values t (make-suffix nil)))
              ((= mismatch prefix-length) (values t (make-suffix mismatch)))
              (t (values nil nil)))))))

(defun ends-with-p (seq suffix &key (test #'char-equal))
  "Returns true if the sequence SEQ ends with the sequence
SUFFIX.  Individual elements are compared with TEST."
  (let ((mismatch (mismatch seq suffix :from-end t :test test)))
    (or (null mismatch)
        (= mismatch (- (length seq) (length suffix))))))

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

If :remove-empty-subseqs is NIL, empty subsequences will be included in the
result; otherwise they will be discarded. All other keywords work analogously
to those for CL:SUBSTITUTE. In particular, the behaviour of :from-end is
possibly different from other versions of this function; :from-end values of
NIL and T are equivalent unless :count is supplied. The second return value is
an index suitable as an argument to CL:SUBSEQ into the sequence indicating
where processing stopped."
  (let ((len (length seq))
        (other-keys 
          (nconc (when test-supplied 
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

If :remove-empty-subseqs is NIL, empty subsequences will be included in the
result; otherwise they will be discarded. All other keywords work analogously
to those for CL:SUBSTITUTE-IF. In particular, the behaviour of :from-end is
possibly different from other versions of this function; :from-end values of
NIL and T are equivalent unless :count is supplied. The second return value is
an index suitable as an argument to CL:SUBSEQ into the sequence indicating
where processing stopped."
  (let ((len (length seq))
        (other-keys (when key-supplied (list :key key))))
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
        (other-keys (when key-supplied (list :key key))))
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
         (mismatch (mismatch subseq seq :test test)))
    (or (null mismatch) (<= length mismatch))))

;; from hunchentoot
(defun starts-with-one-of-p (seq subseq-list &key (test 'eql))
  "Tests whether the sequence SEQ starts with one of the
sequences in SUBSEQ-LIST.  Individual elements are compared with
TEST."
  (some (lambda (subseq) (starts-with-p seq subseq :test test)) subseq-list))

;; matlisp
(definline copy-n (vec lst n)
  "Copy N elements of vector VEC into list LST."
  (declare (type vector vec)
           (type list lst)
           (type fixnum n))
  (loop for i of-type fixnum from 0 below n
        for vlst = lst then (cdr vlst)
        do (setf (car vlst) (aref vec i)))
  lst)

;;; Queues
;;;; Conditions
(defun queue-size-limit-reached (condition stream)
  (let ((queue (error-queue condition))
        (element (error-element condition)))
    (format stream "Size limit (~D) reached for non-extensible ~
                    queue ~S while trying to enqueue element ~S onto it."
            (length (data queue)) queue element)))

(define-condition queue-size-limit-reached (error)
  ((queue :reader error-queue :initarg :queue)
   (element :reader error-element :initarg :element))
  (:report queue-size-limit-reached)
  (:documentation "Error signaled when a queue is saturated."))

;;;; Basic Queue
(defstruct (basic-queue (:conc-name nil)
		                (:constructor %make-basic-queue (head tail)))
  "A basic queue structure."
  (head (error "no head") :type list)
  (tail (error "no tail") :type list))

(defmethod data ((self basic-queue))
  (head self))

(defun make-basic-queue ()
  "Make a BASIC-QUEUE with nil head and tail."
  (%make-basic-queue nil nil))

(defun push-basic-queue (val queue)
  "Push VAL to QUEUE."
  (declare (basic-queue queue))
  (let ((new (cons val nil)))
    (if (head queue)
	    (setf (cdr (tail queue)) new)
	    (setf (head queue) new))
    (setf (tail queue) new)))

(defun pop-basic-queue (queue)
  "Pop the next value off of QUEUE."
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

(definline basic-queue-count (queue) 
  "Return the count of QUEUE."
  (length (the list (head queue))))

(definline basic-queue-empty-p (queue) 
  "Return T if QUEUE is empty."
  (not (head queue)))

(definline peek-basic-queue (queue) 
  "Peek at the next value of QUEUE."
  (let ((node (head queue)))
    (values (car node)
	        (if node t nil))))

;;;; Raw Queue (vectorized)
(deftype raw-queue-count () 
  "The integer type of RAW-QUEUE counts."
  'std/type:array-length)

(defstruct (raw-queue (:constructor %make-raw-queue))
  "A raw queue based on a simple vector."
  (data (vector) :type simple-array)
  (start 0 :type std/type:array-index)
  (count 0 :type raw-queue-count))

(defmethod data ((self raw-queue))
  (raw-queue-data self))

(defun make-raw-queue (capacity &rest args)
  "Return a fresh queue with specified CAPACITY."
  (%make-raw-queue :data (apply 'make-array capacity args)))

(defun push-raw-queue (val queue)
  "Push VAL to QUEUE."
  (declare (raw-queue queue))
  (with-slots (data start count) queue
    (setf (svref data (mod (+ start count) (length data))) val)
    (incf count))
  (values))

(defun pop-raw-queue (queue)
  "Pop the next value off of QUEUE."
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
  "Peek at the next value of QUEUE."
  (declare (raw-queue queue))
  (with-slots (data start count) queue
    (if (plusp count)
        (values (svref data start) t)
        (values nil nil))))

(defun raw-queue-empty-p (queue) 
  "Return T if the QUEUE is empty."
  (declare (raw-queue queue))
  (zerop (raw-queue-count queue)))

(defun raw-queue-full-p (queue) 
  "Return T if the QUEUE is full."
  (declare (raw-queue queue))
  (eql (raw-queue-count queue) (length (raw-queue-data queue))))

(defun raw-queue-capacity (queue) 
  "Return the capacity of QUEUE."
  (declare (raw-queue queue))
  (length (raw-queue-data queue)))

;;;; Vector Queue
;; A thread-safe queue backed by a vector
(defstruct (vector-queue (:constructor %make-vector-queue))
  "A vector queue backed by a primitive queue - defaults to RAW-QUEUE."
  (impl (make-raw-queue 0) :type raw-queue)
  (lock (make-mutex))
  (%push (make-waitqueue :name "%vector-queue-push"))
  (%pop (make-waitqueue :name "%vector-queue-pop")))

(defaccessor data ((self vector-queue))
  (raw-queue-data (vector-queue-impl self)))

(defaccessor lock ((self vector-queue))
  (vector-queue-lock self))

(defun make-vector-queue* (capacity &rest args)
  "Return a fresh VECTOR-QUEUE with specified CAPACITY."
  (%make-vector-queue :impl (apply 'make-raw-queue capacity args)))

(defmacro with-vector-queue-lock (queue &body body)
  "Eval BODY while holding a lock on QUEUE."
  `(with-mutex ((vector-queue-lock ,queue))
     ,@body))

;; no lock
(definline push-vector-queue* (obj queue)
  "Push OBJ to QUEUE without locking Returns the current count of QUEUE before
push."
  (with-slots (impl lock %push %pop) queue
    (let ((count (raw-queue-count impl)))
      (loop (cond ((< count (raw-queue-capacity impl))
		           (push-raw-queue obj impl)
		           (sb-thread:condition-notify %push)
		           (return count))
		          (t
		           (condition-wait %pop lock)))))))

(defun push-vector-queue (obj queue)
  "Push OBJ to QUEUE with locking."
  (declare (vector-queue queue))
  (with-mutex ((vector-queue-lock queue))
    (push-vector-queue* obj queue)
    (values)))

(definline pop-vector-queue* (queue)
  "Pop the next element from QUEUE without locking."
  (declare (vector-queue queue))
  (with-slots (impl lock %push %pop) queue
    (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
	        (cond (presentp
		           (sb-thread:condition-notify %pop)
		           (return value))
		          (t 
		           (condition-wait %push lock)))))))

(defun pop-vector-queue (queue)
  "Pop the next element from QUEUE with locking."
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
                                 %push
				                 lock :timeout time-remaining)))
		          (return (values nil nil)))))))))

(defun %try-pop-vector-queue-with-timeout (queue timeout)
  (with-slots (impl) queue
    (if (raw-queue-empty-p impl)
	    (%try-pop-vector-queue queue timeout)
	    (pop-raw-queue impl))))

(defun try-pop-vector-queue* (queue)
  "Attempt to pop the next element from QUEUE without locking."
  (with-slots (impl %pop) queue
    (multiple-value-bind (value presentp) (pop-raw-queue impl)
      (cond (presentp
             (when %pop (condition-notify %pop))
             (values value t))
            (t (values nil nil))))))

(defun try-pop-vector-queue (queue timeout)
  "Attempt to pop the next element from QUEUE with locking."
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

(defun make-vector-queue (capacity &rest args &key initial-contents &allow-other-keys)
  "Make a new VECTOR-QUEUE with specified CAPACITY and INITIAL-CONTENTS."
  (remf args :initial-contents)
  (let ((queue (apply 'make-vector-queue* capacity args)))
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
;; A thread-safe queue backed by a linked list.
(defstruct (cons-queue (:constructor %make-cons-queue))
  "A cons-based queue backed by a BASIC-QUEUE."
  (impl (make-basic-queue) :type basic-queue)
  (lock (sb-thread:make-mutex))
  (cvar nil))

(defmethod data ((self cons-queue))
  (data (cons-queue-impl self)))

(defmethod next ((self cons-queue))
  (head (data self)))

(defmethod prev ((self cons-queue))
  (tail (data self)))

(defmacro with-cons-queue-lock (queue &body body)
  "Eval BODY while holding a lock on QUEUE."
  `(with-mutex ((cons-queue-lock ,queue))
     ,@body))

(declaim (inline push-cons-queue* pop-cons-queue*))
(defun push-cons-queue* (obj queue) 
  "Push OBJ to QUEUE without locking."
  (declare (cons-queue queue))
  (with-slots (impl cvar) queue
    (push-basic-queue obj impl)
    (when cvar
      (condition-notify cvar)))
  (values))

(defun push-cons-queue (obj queue) 
  "Push OBJ to QUEUE with locking."
  (declare (cons-queue queue))
  (with-mutex ((cons-queue-lock queue))
    (push-cons-queue* obj queue)))

(defun pop-cons-queue* (queue)
  "Pop the next element off QUEUE without locking."
  (declare (cons-queue queue))
  (with-slots (impl lock cvar) queue
    (loop (multiple-value-bind (value presentp) (pop-basic-queue impl)
	        (if presentp
		        (return value)
		        (condition-wait (or cvar (setf cvar (make-waitqueue)))
				                lock))))))

(defun pop-cons-queue (queue) 
  "Pop the next element off QUEUE with locking."
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
  "Attempt to pop the next element from QUEUE waiting at most TIMEOUT seconds."
  (with-slots (impl lock) queue
    (cond ((plusp timeout)
           (with-mutex (lock)
             (try-pop-cons-queue-with-timeout queue timeout)))
          (t
           ;; optimization: don't lock if nothing is there
           (if (basic-queue-empty-p impl)
               (return-from try-pop-cons-queue (pop-basic-queue impl))
               (pop-cons-queue queue))
           (values nil nil)))))

(defun try-pop-cons-queue* (queue timeout)
  "Attempt to pop the next element from QUEUE, waiting at most TIMEOUT seconds
for a lock before calling without it."
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
  "Make a new CONS-QUEUE with INITIAL-CONTENTS."
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
(defvar *default-priority* 0
  "The default priority of elements pushed to a PRIORITY-QUEUE.")
(defvar *default-priority-queue-size* 256
  "The default size of a PRIORITY-QUEUE.")
(deftype priority () '(unsigned-byte 32))
(deftype priority-vector () '(simple-array priority (*)))
(deftype priority-vector-extension () '(integer 2 256))

(defstruct (priority-queue (:constructor %make-priority-queue))
  "An (optionally) adjustable Priority Queue backed by a data vector and
associated priority vector."
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

(declaim (ftype (function (t priority-queue &optional priority) (values null &optional)) push-priority-queue))
(definline push-priority-queue (object queue &optional (priority *default-priority*))
  "Push OBJECT to QUEUE with supplied PRIORITY."
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

(declaim (ftype (function (simple-array priority-vector (array-index))
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
  "Pop the next element from QUEUE."
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

(defun make-priority-queue (capacity &key initial-contents prioritize (element-type t) initial-element (extend t))
  "Make a new PRIORITY-QUEUE with specified CAPACITY."
  (let ((queue (%make-priority-queue
                :data (make-array capacity :element-type element-type :initial-element initial-element)
                :priorities (make-array capacity :element-type 'priority)
                :extend-p extend)))
    (setf (priority-queue-size queue) capacity)
    (when initial-contents
      (flet ((push-elem (elem)
               (push-priority-queue elem queue (if prioritize (funcall prioritize elem) *default-priority*))))
        (declare (dynamic-extent #'push-elem))
        (map nil #'push-elem initial-contents)))
    queue))

;;; Fibonacci Queue
;; fibonacci heap implementation (min-priority queue)
(defstruct hnode
  (id -1 :type fixnum)
  (degree 0 :type fixnum)
  (mark nil :type boolean)
  (parent nil :type (or null hnode))
  (children nil :type list)
  (data nil :type list) ;dcons
  key)

(defmethod data ((self hnode)) (hnode-data self))

(defmethod print-object ((nd hnode) stream)
  (print-unreadable-object (nd stream :type t)
    (format stream ":key ~A :degree ~A :mark ~A" (hnode-key nd) (hnode-degree nd) (hnode-mark nd))))

(definline dappend2 (a b)
  (declare (type list a b))
  (cond
    ((null a) b) ((null b) a)
    (t (rotatef (first a) (first b))
       (rotatef (second (first a)) (second (first b)))
       a)))

(defstruct fib-heap
  (root nil)
  (trees 0)
  (elements 0)
  (order #'<)
  (data (make-hash-table)))

(defmethod data ((self fib-heap)) (fib-heap-data self))

(defmethod print-object ((fib fib-heap) stream)
  (print-unreadable-object (fib stream :type t)
    (format stream ":size ~A :trees ~A" (fib-heap-elements fib) (fib-heap-trees fib))))

(defun make-heap (&optional (order #'<) (test #'eql))
  (make-fib-heap :order order :data (make-hash-table :test test)))

(definline fib-order (a b fib)
  (funcall (slot-value fib 'std/seq::order) a b))

(definline min-key (fib)
  (when (fib-heap-root fib)
    (values (hnode-key (dcar (fib-heap-root fib))) (hnode-id (dcar (fib-heap-root fib))))))

(definline node-existsp (id fib)
  (letv* ((node exists-p (gethash id (fib-heap-data fib))))
    (and exists-p (hnode-data node) t)))

(defun fib-insert (key fib &optional id)
  (letv* ((pmin mid (min-key fib))
          (node (make-hnode :key key :id (or id (fib-heap-elements fib)))))
    (assert (not (node-existsp (hnode-id node) fib)) nil "id already exists in the heap")
    (setf (hnode-data node) (dpush node (fib-heap-root fib)))
    (unless (and mid (fib-order key pmin fib))
      (setf (fib-heap-root fib) (dcdr (fib-heap-root fib))))
    (setf (gethash (hnode-id node) (fib-heap-data fib)) node)
    (incf (fib-heap-elements fib))
    (incf (fib-heap-trees fib))
    (hnode-id node)))

(definline node-key (id fib)
  (std/macs:letv* ((node exists-p (gethash id (fib-heap-data fib))))
    (when exists-p (hnode-key node))))

(definline (setf node-key) (value id fib)
  (decrease-key id value fib))

(defun extract-min (fib)
  (let ((z (dpop (fib-heap-root fib))))
    ;;move the children of z into root
    (when z
      (decf (fib-heap-elements fib))
      (decf (fib-heap-trees fib))
      (let ((i 0))
        (within-dlist (node (hnode-children z))
          (incf i)
          (setf (hnode-parent node) nil))
        (incf (fib-heap-trees fib) i))
      (setf (fib-heap-root fib) (dappend2 (hnode-children z) (fib-heap-root fib)))
      (setf (hnode-children z) nil
            (hnode-degree z) 0
            (hnode-data z) nil))
    (when (> (fib-heap-trees fib) 1)
      ;;consolidate
      (let ((an (make-array (+ 2 (integer-length (fib-heap-elements fib))) :initial-element nil)))
        (with-dlist (w (fib-heap-root fib))
          (let ((fw (list t t)))
            ;;This hack allows for destructive updates. See the iterate defclause in dlist.lisp
            (setf (second fw) (second w))
            (loop with x = w
                  with d = (hnode-degree (dcar x))
                  while (aref an d)
                  do (let ((y (aref an d)))
                       (when (fib-order (hnode-key (dcar y)) (hnode-key (dcar x)) fib)
                         (rotatef y x))
                       ;; fib-heap-link
                       (let ((y.node (dpop y)))
                         (setf (fib-heap-root fib) y
                               (hnode-parent y.node) (dcar x)
                               (hnode-mark y.node) nil)
                         (decf (fib-heap-trees fib))
                         (setf (hnode-children (dcar x)) (dappend2 (hnode-data y.node) (hnode-children (dcar x))))))
                  do (progn
                       (setf (aref an d) nil)
                       
                       (incf d)
                       (setf (hnode-degree (dcar x)) d))
                  finally (setf (aref an d) x))
            (setq w fw)))
        ;; update min
        (let ((fmin nil))
          (with-dlist (rot (fib-heap-root fib))
            (when (or (null fmin) (fib-order (hnode-key (dcar rot)) (hnode-key (dcar fmin)) fib))
              (setf fmin rot)))
          (setf (fib-heap-root fib) fmin))))
    (when z
      (values (hnode-key z) (hnode-id z)))))

(definline cut (x y fib)
  "cut T_x from δ(y)"
  (decf (hnode-degree y))
  (setf (hnode-children y) (let ((tmp (hnode-data x))) (dpop tmp) tmp)
        (hnode-parent x) nil
        (hnode-mark x) nil)
  (incf (fib-heap-trees fib))
  (dappend2 (hnode-data x) (fib-heap-root fib)))

(defun ccut (y fib)
  "cascading cut"
  (when-let ((z (hnode-parent y)))
    (if (hnode-mark y)
        (progn (cut y z fib) (ccut z fib))
        (setf (hnode-mark y) t))))

(defun decrease-key (id key fib)
  (let ((node (gethash id (fib-heap-data fib))))
    (declare (type hnode node))
    (assert (not (fib-order (hnode-key node) key fib)) (key) 'invalid-item :reason "new key is greater than the current." :item key)
    (setf (hnode-key node) key)
    (when (hnode-data node)
      ;;cut node
      (let ((y (hnode-parent node)))
        (when (and y (fib-order key (hnode-key y) fib))
          (cut node y fib) (ccut y fib)))
      ;;update min
      (when (fib-order key (min-key fib) fib)
        (setf (fib-heap-root fib) (hnode-data node)))))
  key)

(defun fib-delete (id fib &optional delete?)
  (let ((node (gethash id (fib-heap-data fib))))
    (declare (type hnode node))
    (when (hnode-data node)
      ;;cut node
      (let ((y (hnode-parent node)))
        (when y (cut node y fib) (ccut y fib)))
      ;;move to root
      (setf (fib-heap-root fib) (hnode-data node))
      (extract-min fib))
    (when delete? (remhash id (fib-heap-data fib))))
  id)

;;; Pqueue
;; more like FIB-HEAP than PRIORITY-QUEUE, but based on vectors instead of
;; dlists. Note that this is already implemented in SBCL internally (see
;; timer.lisp) but is not accessible at runtime :(
;;;; Heap
(definline heap-parent (i)
  (ash (1- i) -1))

(definline heap-left (i)
  (1+ (ash i 1)))

(definline heap-right (i)
  (+ 2 (ash i 1)))

(definline heap-size (heap)
  (1- (length heap)))

(defun heapify (heap start &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (let ((l (heap-left start))
          (r (heap-right start))
          (size (heap-size heap))
          largest)
      (setf largest (if (and (<= l size)
                             (not (ge (key (aref heap start))
                                      (key (aref heap l)))))
                        l
                        start))
      (when (and (<= r size)
                 (not (ge (key (aref heap largest))
                          (key (aref heap r)))))
        (setf largest r))
      (when (/= largest start)
        (rotatef (aref heap largest) (aref heap start))
        (heapify heap largest :key key :test test)))
    heap))

(definline heap-insert (heap new-item &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (vector-push-extend nil heap)
    (loop for i = (heap-size heap) then parent-i
          for parent-i = (heap-parent i)
          while (and (> i 0)
                     (not (ge (key (aref heap parent-i))
                              (key new-item))))
          do (setf (aref heap i) (aref heap parent-i))
          finally (setf (aref heap i) new-item)
                  (return-from heap-insert i))))

(defun heap-maximum (heap)
  (unless (zerop (length heap))
    (aref heap 0)))

(defun heap-extract (heap i &key (key #'identity) (test #'>=))
  (unless (> (length heap) i)
    (error "Heap underflow"))
  (prog1
      (aref heap i)
    (setf (aref heap i) (aref heap (heap-size heap)))
    (decf (fill-pointer heap))
    (heapify heap i :key key :test test)))

(defun heap-extract-maximum (heap &key (key #'identity) (test #'>=))
  (heap-extract heap 0 :key key :test test))

(defstruct (pqueue (:conc-name %pqueue-) (:constructor %make-pqueue))
  "A simplified Priority Queue similar to the struct of the same name internal to SBCL
timers."
  contents
  keyfun)

(defun make-pqueue (&key (key #'identity) (element-type t))
  "Make a new PQUEUE."
  (%make-pqueue 
   :keyfun key 
   :contents (the vector (make-array 100 :adjustable t :fill-pointer 0 :element-type element-type))))

(defmethod print-object ((object pqueue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~[empty~:;~:*~D item~:P~]"
            (length (%pqueue-contents object)))))

(defmethod data ((self pqueue)) 
  "Return the contents of a PQUEUE."
  (%pqueue-contents self))

(defun pqueue-maximum (pq)
  "Return the item in PRIORITY-QUEUE with the largest key."
  (symbol-macrolet ((contents (%pqueue-contents pq)))
    (unless (zerop (length contents))
      (heap-maximum contents))))

(defun pqueue-extract-maximum (pq)
  "Remove and return the item in PRIORITY-QUEUE with the largest key."
  (symbol-macrolet ((contents (%pqueue-contents pq))
                    (keyfun (%pqueue-keyfun pq)))
    (unless (zerop (length contents))
      (heap-extract-maximum contents :key keyfun :test #'<=))))

(defun pqueue-insert (pq new-item)
  "Add NEW-ITEM to PRIORITY-QUEUE."
  (symbol-macrolet ((contents (%pqueue-contents pq))
                    (keyfun (%pqueue-keyfun pq)))
    (heap-insert contents new-item :key keyfun :test #'<=)))

(defun pqueue-empty-p (pq)
  (zerop (length (%pqueue-contents pq))))

(defun pqueue-remove (pq item &key (test #'eq))
  "Remove and return ITEM from PRIORITY-QUEUE."
  (symbol-macrolet ((contents (%pqueue-contents pq))
                    (keyfun (%pqueue-keyfun pq)))
    (let ((i (position item contents :test test)))
      (when i
        (heap-extract contents i :key keyfun :test #'<=)
        i))))

(defun pqueue-reorder (pq)
  (symbol-macrolet ((contents (%pqueue-contents pq))
                    (keyfun (%pqueue-keyfun pq)))
    (heapify contents 0 :key keyfun :test #'<=)))

;;; Spin Queue
(defconstant +dummy+ :null
  "Dummy SPIN-QUEUE value.")

(defconstant +dead-end+ :dead
  "Dead-end value for SPIN-QUEUEs.")

(defun make-spin-lock () 
  "Allocate a fresh 'spin-lock' which is simply NIL."
  nil)

(defstruct (spin-queue (:constructor %make-spin-queue (head tail)))
  "CAS-based spin-lock queue."
  (head (error "no head") :type cons)
  (tail (error "no tail") :type cons))

(defun make-spin-queue ()
  "Make a fresh SPIN-QUEUE."
  (let ((dummy (cons +dummy+ nil)))
    (%make-spin-queue dummy dummy)))

(defonce with-spin-lock ((access &once container) &body body)
  "Eval BODY within a CAS-based spin-lock which calls ACCESS against CONTAINER
until the value is non-nil."
  `(locally (declare (optimize (speed 3) (safety 0)))
     (loop until (sb-ext:cas (,access ,container) nil t))
     (unwind-protect (progn ,@body)
       (setf (,access ,container) nil))))

(defun push-spin-queue (value queue) 
  "Push VALUE onto QUEUE."
  (declare (ftype (function (t spin-queue) (values)) push-spin-queue))
  ;; Attempt CAS, repeat upon failure. Upon success update QUEUE-TAIL.
  (let ((new (cons value nil)))
    (loop (when (sb-ext:cas (cdr (spin-queue-tail queue)) nil new)
            (setf (spin-queue-tail queue) new)
            (return (values))))))

(defun pop-spin-queue (queue) 
  "Attempt to CAS QUEUE-HEAD with the next node, repeat upon failure. Upon
success, clear the discarded node and set the CAR of QUEUE-HEAD to +DUMMY+."
  (declare (ftype (function (spin-queue) (values t boolean))))
  (loop (let* ((head (spin-queue-head queue))
               (next (cdr head)))
          ;; NEXT could be +DEAD-END+, whereupon we try again.
          (typecase next
            (null (return (values nil nil)))
            (cons (when (sb-ext:cas (spin-queue-head queue) head next)
                    (let ((value (car next)))
                      (setf (cdr head) +dead-end+
                            (car next) +dummy+)
                      (return (values value t)))))))))

(defun spin-queue-empty-p (queue)
  "Return T if QUEUE is empty."
  (null (cdr (spin-queue-head queue))))

(defun try-each-elem (fun queue)
  "Try FUN on each element of QUEUE."
  (declare ((function (t) (values t boolean)) fun))
  (let ((node (spin-queue-head queue)))
    (loop
      (let ((value (car node)))
        (unless (eq value +dummy+)
          (funcall fun value)))
      (setf node (cdr node))
      (cond 
        ((eq node +dead-end+)
         (return nil))
        ((null node)
         (return t))))))

(defun spin-queue-count (queue)
  "Return the count of QUEUE."
  (tagbody
   :retry
     (let ((count 0))
       (declare (fixnum count))
       (unless (try-each-elem
                (lambda (elem)
                  (declare (ignore elem))
                  (incf count))
                queue)
         (go :retry))
       (return-from spin-queue-count count))))

(defun peek-spin-queue (queue)
  "Peek at the next element of QUEUE."
  (declare (optimize (safety 0)))
  (loop 
    until (try-each-elem 
           (lambda (elem)
             (return-from peek-spin-queue (values elem t)))
           queue))
  (values nil nil))

;;;; Protocol
(deftype queue () 
  "Queue type spec."
  '(or cons-queue vector-queue raw-queue basic-queue priority-queue spin-queue))

(defun make-queue (&key capacity initial-contents prioritize initial-element (element-type t) extend)
  "Make a new queue."
  (cond 
    ((and capacity (not prioritize)) (make-vector-queue capacity :initial-contents initial-contents :initial-element initial-element))
    ((not prioritize) (make-cons-queue :initial-contents initial-contents))
    (prioritize (make-priority-queue (or capacity *default-priority-queue-size*) 
                                     :initial-contents initial-contents 
                                     :prioritize prioritize 
                                     :initial-element initial-element 
                                     :element-type element-type
                                     :extend (and (or extend capacity) t)))))

(defun call-with-cons-queue-lock (fn queue)
  "Call FN with a lock on QUEUE."
  (with-cons-queue-lock queue
    (funcall fn)))

(defun call-with-vector-queue-lock (fn queue)
  "Call FN with a lock on QUEUE."
  (with-vector-queue-lock queue
    (funcall fn)))

(defmacro with-queue-lock (queue &body body)
  "Eval BODY with a lock on QUEUE."
  `(call-with-queue-lock (lambda () ,@body) ,queue))

(defun cons-queue-full-p (queue) 
  "A CONS-QUEUE is never full so this is always a no-op."
  (declare (ignore queue)) 
  nil)

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

;;; Ring Buffer

;;; Accumulator

;; originally part of q/query, may serve useful in other contexts.

(defclass accumulator ()
  ((value :initarg :value :accessor accumulated))
  (:documentation "Accumulator superclass."))

(defgeneric accumulate (self val)
  (:documentation "Accumulate VAL into an ACCUMULATOR-like object SELF.")
  (:method ((self accumulator) (val number))
    (setf (accumulated self) (+ val (accumulated self))))
  (:method ((self list) val)
    (push val self))
  (:method ((self vector) val)
    (vector-push val self)))

(defgeneric make-accumulator (self)
  (:documentation "Make a new ACCUMULATOR based on SELF."))

;; max-accumulator
(defclass max-accumulator (accumulator) ()
  (:documentation "Accumulator which tracks the maximum value observed.")
  (:default-initargs :value 0))

(defmethod accumulate ((self max-accumulator) (val number))
  (when (> val (accumulated self))
    (setf (accumulated self) val)))

(defclass min-accumulator (accumulator) ()
  (:documentation "Accumulator which tracks the minimum value observed.")
  (:default-initargs :value 0))

(defmethod accumulate ((self min-accumulator) (val fixnum))
  (when (< val (accumulated self))
    (setf (accumulated self) val)))

;; simple atomic counter
(defstruct (counter (:constructor make-counter (&optional value)))
  (value 0 :type sb-ext:word))
(defun inc-counter (c &optional (diff 1))
  (declare (counter c) ((signed-byte 64) diff))
  (sb-ext:atomic-incf (counter-value c) diff))
(defun dec-counter (c &optional (diff 1))
  (declare (counter c) ((signed-byte 64) diff))
  (sb-ext:atomic-decf (counter-value c) diff))

;;; Iterator
#|

The iterator protocol allows subsequently accessing some or all elements of a
sequence in forward or reverse direction. Users first call
make-sequence-iterator to create an iteration state and receive functions to
query and mutate it. These functions allow, among other things, moving to,
retrieving or modifying elements of the sequence. An iteration state consists
of a state object, a limit object, a from-end indicator and the following six
functions to query or mutate this state: step endp element (setf element)
index copy

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
        (incf *idx*)))
    (:method ((self list))
      (prog1 (nth *idx* self)
        (incf *idx*))))
  (defgeneric idx (self)
    (:method ((self t)) *idx*))
  (defgeneric prev (self)
    (:method ((self array))
      (if (zerop *idx*)
          (aref self (length self))
          (aref self (- (decf *idx*) 1))))
    (:method ((self list))
      (if (zerop *idx*)
          (elt self (length self))
          (elt self (- (decf *idx*) 1)))))
  (defgeneric key (self))
  (defgeneric (setf key) (new self))
  (defgeneric val (self)
    (:method ((self null)) self))
  (defgeneric (setf val) (new self))
  (defgeneric iter (self &key &allow-other-keys))
  (defgeneric iter-valid-p (self)
    (:method ((self sequence))
      (and *idx* (> (length self) *idx*))))
  (defgeneric seek (self key &key)
    (:method ((self array) key &key test)
      (when-let ((i (position key self :test test :start *idx*)))
        (setf *idx* i)
        (aref self *idx*)))
    (:method ((self list) key &key test)
      (when-let ((i (position key self :test test :start *idx*)))
        (setf *idx* i)
        (elt self *idx*))))
  (defgeneric seek-to-first (self)
    (:method ((self sequence))
      (setf *idx* 0)
      (values)))
  (defgeneric seek-to-last (self)
    (:method ((self array))
      (setf *idx* (length self))
      (aref self *idx*))
    (:method ((self list))
      (setf *idx* (length self))
      (elt self *idx*)))
  (defgeneric seek-for-prev (self key &key)
    (:method ((self array) key &key test)
      (when-let ((i (position key self :end *idx* :test test)))
        (setf *idx* i)
        (aref self *idx*)))
    (:method ((self list) key &key test)
      (when-let ((i (position key self :end *idx* :test test)))
        (setf *idx* i)
        (elt self *idx*)))))

(defvar *iter*)

(defvar *iterator-functions*
  '((next (next *iter*))
    (prev (prev *iter*))
    (seek-to-first (seek-to-first *iter*))
    (seek-to-last (seek-to-last *iter*))
    ;; (seek-for-prev (key &optional (s *iter*)) (seek-for-prev s key))
    (iter-valid-p (iter-valid-p *iter*))
    ;; (seek (key &optional (s *iter*)) (seek s key))
    (val (val *iter*))
    (key (key *iter*))
    (idx (idx *iter*)))
  "A list of function signatures for symbols which are bound via SYMBOL-MACROLET
around the body of WITH-ITER.")

(defmacro with-iter ((sym iter) &body body)
  `(let ((,sym ,iter)
         (*idx* 0))
     (setf *iter* ,sym)
     (symbol-macrolet ,*iterator-functions*
       ;; (declare (ignorable ,@(mapcar (lambda (x) `(function ,(car x))) *iterator-functions*)))
       ,@body)))

;;; Util
(defmacro mod-inc (k n)
  "(1+k) mod n."
  `(the array-index (mod (the array-index (1+ (the array-index ,k)))
                         (the array-index ,n))))

(defmacro mod-dec (k n)
  "(1-k) mod n."
  `(the array-index (mod (the fixnum (1- (the array-index ,k)))
                         (the array-index ,n))))

(defmacro mod-incf (place n)
  `(the array-index (setf ,place (mod-inc ,place ,n))))

(defmacro mod-decf (place n)
  `(the array-index (setf ,place (mod-dec ,place ,n))))

(defmacro repeat (n &body body)
  "Repeat BODY N times."
  (with-gensyms (left)
    `(let ((,left (the fixnum ,n)))
       (declare (type fixnum ,left))
       (loop
         (when (zerop ,left)
           (return (values)))
         (decf ,left)
         ,@body))))

(defmacro do-indexes ((ivar size hindex from-hindex-p) &body body)
  "Eval BODY once for each positive integer of SIZE with IVAR being a symbol
bound to the current index. HINDEX is the starting location. When
FROM-HINDEX-P is NIL, instead of starting at HINDEX we end with it - such that
the last binding of IVAR = HINDEX.

(do-indexes (i 20 4 nil) (print i)) ; ends at 4
(do-indexes (i 20 4 t) (print i)) ; starts at 4, ends at 3"
  ;; size is positive
  (with-gensyms (svar hivar)
    `(let ((,ivar (the array-index ,hindex))
           (,svar (the array-index ,size))
           (,hivar (the array-index ,hindex)))
       (declare (type array-index ,ivar ,svar ,hivar))
       (loop
            ,(let ((next `(mod-incf ,ivar ,svar)))
               (if from-hindex-p
                   `(progn ,@body ,next)
                   `(progn ,next ,@body)))
            (when (= ,ivar ,hivar)
              (return (values)))))))
