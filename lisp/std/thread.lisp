;;; threads.lisp --- Multi-thread utilities

;; Threading Macros

;;; Commentary:

;; mostly yoinked from sb-thread and friends

;;; Code:
(in-package :std/thread)

;; (sb-thread:thread-os-tid sb-thread:*current-thread*)
;; sb-thread:interrupt-thread

(defun thread-support-p () (member :thread-support *features*))

(eval-when (:compile-toplevel)
  (defun print-thread-message-top-level (msg)
    (sb-thread:make-thread
     (lambda ()
       (format #.*standard-output* msg)))
    nil))

;; this is all very unsafe. don't touch the finalizer thread plz.
(defun find-thread-by-id (id)
  "Search for thread by ID which must be an u64. On success returns the thread itself or nil."
  (sb-thread::avlnode-data (sb-thread::avl-find id sb-thread::*all-threads*)))

(defun thread-id-list ()
  (sb-thread::avltree-filter #'sb-thread::avlnode-key sb-thread::*all-threads*))

(defun thread-count ()
  (sb-thread::avl-count sb-thread::*all-threads*))

(defun make-threads (n fn &key (name "thread"))
  (declare (type fixnum n))
  (loop for i below n
        collect (make-thread fn :name (format nil "~A-~D" name i))))

(defmacro with-threads ((idx n) &body body)
  `(make-threads ,n (lambda (,idx) (declare (ignorable ,idx)) ,@body)))

(defun finish-threads (&rest threads)
  (let ((threads (flatten threads)))
    (unwind-protect
         (mapc #'join-thread threads)
      (dolist (thread threads)
        (when (thread-alive-p thread)
          (terminate-thread thread))))))

(defun timed-join-thread (thread timeout)
  (declare (type thread thread) (type float timeout))
  (handler-case (sb-sys:with-deadline (:seconds timeout)
                  (join-thread thread :default :aborted))
    (sb-ext:timeout ()
      :timeout)))

(defun hang ()
  (join-thread *current-thread*))

(defun kill-thread (thread)
  (when (thread-alive-p thread)
    (ignore-errors
      (terminate-thread thread))))

;; (sb-vm::primitive-object-slots (sb-vm::primitive-object 'sb-vm::thread))
(defun init-session (&optional (thread *current-thread*)) (sb-thread::new-session thread))

;; (sb-thread::with-progressive-timeout (timet :seconds 4) (dotimes (i 4000) (print (timet))))

;; (describe sb-thread::*session*)

;; make-listener-thread 

;; with-progressive-timeout

;; from sb-thread
(defun dump-thread ()
  (let* ((slots (sb-vm::primitive-object-slots #1=(sb-vm::primitive-object 'sb-vm::thread)))
         (sap (current-thread-sap))
         (thread-obj-len (sb-vm::primitive-object-length #1#))
         (names (make-array thread-obj-len :initial-element "")))
    (loop for slot across slots
          do
          (setf (aref names (sb-vm::slot-offset slot)) (sb-vm::slot-name slot)))
    (flet ((safely-read (sap offset &aux (bits (sb-vm::sap-ref-word sap offset)))
             (cond ((eql bits sb-vm:no-tls-value-marker) :no-tls-value)
                   ((eql (logand bits sb-vm:widetag-mask) sb-vm:unbound-marker-widetag) :unbound)
                   (t (sb-vm::sap-ref-lispobj sap offset))))
           (show (sym val)
             (declare (type fixnum sym))
             (let ((*print-right-margin* 128)
                   (*print-lines* 4))
               (format t " ~3d ~30a : ~s~%"
                       #+sb-thread (ash sym (- sb-vm:word-shift))
                       #-sb-thread 0
                       #+sb-thread (sb-vm:symbol-from-tls-index sym)
                       #-sb-thread sym
                       val))))
      (format t "~&TLS: (base=~x)~%" (sb-vm::sap-int sap))
      (loop for tlsindex from sb-vm:n-word-bytes below
            #+sb-thread (ash sb-vm::*free-tls-index* sb-vm:n-fixnum-tag-bits)
            #-sb-thread (ash thread-obj-len sb-vm:word-shift)
            by sb-vm:n-word-bytes
            do
         (unless (<= sb-vm::thread-allocator-histogram-slot
                     (ash tlsindex (- sb-vm:word-shift))
                     (1- sb-vm::thread-lisp-thread-slot))
           (let ((thread-slot-name
                  (if (< tlsindex (ash thread-obj-len sb-vm:word-shift))
                           (aref names (ash tlsindex (- sb-vm:word-shift))))))
                 (if (and thread-slot-name (sb-vm::neq thread-slot-name 'sb-vm::lisp-thread))
                     (format t " ~3d ~30a : #x~x~%" (ash tlsindex (- sb-vm:word-shift))
                             thread-slot-name (sb-vm::sap-ref-word sap tlsindex))
                     (let ((val (safely-read sap tlsindex)))
                       (unless (eq val :no-tls-value)
                         (show tlsindex val)))))))
      (let ((from (sb-vm::descriptor-sap sb-vm:*binding-stack-start*))
            (to (sb-vm::binding-stack-pointer-sap)))
        (format t "~%Binding stack: (depth ~d)~%"
                (/ (sb-vm::sap- to from) (* sb-vm:binding-size sb-vm:n-word-bytes)))
        (loop
          (when (sb-vm::sap>= from to) (return))
          (let ((val (safely-read from 0))
                (sym #+sb-thread (sb-vm::sap-ref-word from sb-vm:n-word-bytes) ; a TLS index
                     #-sb-thread (sb-vm::sap-ref-lispobj from sb-vm:n-word-bytes)))
            (show sym val))
          (setq from (sb-vm::sap+ from (* sb-vm:binding-size sb-vm:n-word-bytes))))))))

(defun wait-for-threads (threads)
  (map 'list (lambda (thread) (sb-thread:join-thread thread :default nil)) threads)
  (not (some #'sb-thread:thread-alive-p threads)))

(defun process-all-interrupts (&optional (thread sb-thread:*current-thread*))
  (sb-ext:wait-for (null (sb-thread::thread-interruptions thread))))

;;; Tasks
(defclass supervisor ()
  (scope)
  (:documentation "A class which provides a view of the work done within a specified
SCOPE.

This object should be used by operators to inspect 'runstreams'
performed in other threads, such as WORKERS in TASK-POOL.

Before using this object you should ensure the SCOPE is fully
initialized. Supervisors should be created at any point during the
lifetime of SCOPE, but never before and never after."))
(thread-id-list)
;; unix-getrusage  
;; 0,-1,-2
;; (multiple-value-list (sb-unix:unix-getrusage 0))
;; (setf sb-unix::*on-dangerous-wait* :error)
(defvar *oracle-threads* nil)

(defun find-oracle (id)
  (declare ((unsigned-byte 32) id))
  (find id *oracle-threads* :test '= :key 'oracle-id))

(defstruct (oracle (:constructor %make-oracle (id thread)))
  (id 0 :type (unsigned-byte 32) :read-only t)
  (thread *current-thread* :read-only t))

(defun make-oracle (thread)
  (let ((orc (%make-oracle (sb-thread:thread-os-tid thread) thread)))
    (prog1 orc
      (pushnew orc *oracle-threads* :test '= :key #'oracle-id))))

(defgeneric designate-oracle (host guest))

(defgeneric push-job (job pool))
(defgeneric push-task (task pool))
(defgeneric push-result (task pool))
(defgeneric push-worker (thread pool))
(defgeneric push-workers (threads pool))
(defgeneric push-stage (stage pool))
(defgeneric find-job (job pool &key &allow-other-keys))

(defgeneric delete-job (job pool &key &allow-other-keys))
(defgeneric pop-job (pool))
(defgeneric pop-task (pool))
(defgeneric pop-result (pool))
(defgeneric pop-worker (pool))
(defgeneric pop-stage (pool))

(defgeneric start-task-pool (pool))
(defgeneric pause-task-pool (pool))
(defgeneric stop-task-pool (pool))
(defgeneric make-task (&rest args))
(defgeneric run-job (self job))
(defgeneric run-stage (self stage))
(defgeneric run-task (self task))

(defgeneric make-worker-for (pool function &rest args)
  (:method ((pool null) (function function) &rest args)
    (declare (ignore pool))
    (make-thread function :arguments args)))

(defvar *default-worker-name* "worker")

(defgeneric make-workers-for (pool count function)
  (:method ((pool null) (count fixnum) (function function))
    (declare (ignore pool))
    (make-threads count function :name *default-worker-name*)))

(defmacro define-task-kernel (name (&key args accessors) &body body)
  "Define a task kernel.

(define-task-kernel NAME (&key ARGS MAX MIN ACCESSORS)

The kernel should process all options and return a function - the
'kernel function'.

The kernel function is installed in worker threads by passing it to
SB-THREAD:MAKE-THREAD. It may accept a varying number of arguments
specified by ARGS.

ACCESSORS is a list of pandoric accessors which can be called on the
kernel via an ORACLE. 

This interface is experimental and subject to change."
  `(defun ,name (,@args) 
     ,@body))

(define-task-kernel default-task-kernel (:args () )
  "The default task kernel used to initialize the KERNEL slot of
task-pools.

"
  nil)

(defgeneric spawn-worker (pool)
  (:method ((pool null))
    (declare (ignore pool))
    (make-thread (default-task-kernel))))

(defgeneric spawn-workers (pool count)
  (:method ((pool null) (count fixnum))
    (declare (ignore pool))
    (make-threads count (default-task-kernel) :name *default-worker-name*)))

(defstruct task-pool
  (oracle-id nil :type (or null (unsigned-byte 32)))
  (kernel #'default-task-kernel :type function)
  (jobs (make-queue :name "jobs"))
  (stages (make-array 0 :element-type 'stage :fill-pointer 0) :type (array stage *))
   ;; When open, indicates that the pool is fully initialized and workers
   ;; may make progress.
  (online (make-gate :name "online" :open nil)
   :type gate)
  ;; TODO: test weak-vector here
  (workers (make-array 0 :element-type '(unsigned-byte 32) :fill-pointer 0) :type (vector (unsigned-byte 32) *))
  (results (make-mailbox :name "results")))

(defmethod designate-oracle ((self task-pool) (guest integer))
  (setf (task-pool-oracle-id self) guest)
  self)

(defmethod designate-oracle ((self task-pool) (guest thread))
  (designate-oracle self (make-oracle guest)))

(defmethod task-pool-oracle ((self task-pool))
  (oracle-thread (find-oracle (slot-value self 'oracle))))

(defmethod push-worker ((worker thread) (pool task-pool))
  (vector-push (thread-os-tid worker) (task-pool-workers pool)))

(defmethod push-workers ((threads list) (pool task-pool))
  (with-slots (workers) pool
    (dolist (w threads)
      (vector-push (thread-os-tid w) workers))))

(defmethod make-worker-for ((pool task-pool) function &rest args)
  (make-thread function :name *default-worker-name* :arguments args))

(defmethod make-workers-for ((pool task-pool) (count fixnum) function)
  (make-threads count function :name *default-worker-name*))

(defmethod spawn-worker ((pool task-pool))
  ;; (with-recursive-lock
  (push-worker (make-worker-for pool (task-pool-kernel pool)) pool))

(defmethod spawn-workers ((pool task-pool) (count fixnum))
  (push-workers (make-workers-for pool count (task-pool-kernel pool)) pool))

(defclass task ()
  ((state :initarg :state :accessor task-state)
   (object :initarg :object :accessor task-object))
  (:documentation "This object represents a single unit of work to be done by some
worker. Tasks are typically generated by an oracle, but workers may
also be granted the ability to create and distribute their own
tasks. Once a task is assigned, the 'owner', i.e. the worker that is
assigned this task, may modify the object and state. When the work
associated with a task is complete, the owner is responsible for
indicating in the state slot the result of the computation."))

(defmethod make-task (&rest args)
  (make-instance 'task :object args))

(defmethod print-object ((self task) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (task-object self))))

(defmethod push-result ((task task) (pool task-pool))
  (send-message (task-pool-results pool) task))

(defmethod run-task ((self thread) (task task))
  )

(defstruct (job (:constructor %make-job (tasks)))
  "A collection of tasks to be performed by worker threads."
  (tasks (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
   :type (array task *))
  (lock (make-mutex :name "job") :type mutex))

(defgeneric make-job (self &key &allow-other-keys))

(defmethod make-job ((self task) &key (size 1))
  (%make-job (make-array size :element-type 'task
                              :initial-element self)))

(defmethod make-job ((self vector) &key)
  (%make-job self))

(defmethod make-job ((self null) &key (size 1))
  (%make-job (make-array size :element-type 'task :fill-pointer 0 :adjustable t)))

(defmethod print-object ((self job) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (job-tasks self))))

(defmethod push-task ((task task) (job job))
  (vector-push task (job-tasks job)))

(defmethod push-task ((task task) (pool task-pool))
  (push-job (make-job task) pool))

(defmethod push-job ((job job) (pool task-pool))
  (enqueue job (task-pool-jobs pool)))

;; TODO..
(defmethod run-job ((self task-pool) (job job))
  #+log (log:trace! "running remote job...")
  (push-job job self))

(defclass stage ()
  ((jobs  :initform (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
          :initarg :jobs
          :accessor jobs
          :type (vector job))
   (lock :initform (make-mutex :name "stage") :initarg :lock :accessor stage-lock :type mutex)))

(defmethod print-object ((self stage) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (jobs self))))

(defmethod push-stage ((stage stage) (pool task-pool))
  (vector-push stage (task-pool-stages pool)))

(defmethod run-stage ((self thread) (stage stage)))
