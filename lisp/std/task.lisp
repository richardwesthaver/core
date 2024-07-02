;;; task.lisp --- Standard Task API

;; 

;;; Code:
(in-package :std/task)

;;; Vars
(defvar *task-pool*)
(defvar *tasks*)
(defvar *workers*)
(defvar *jobs*)
(defvar *stages*)
(defvar *oracles* nil)

;;; Kernel
(defmacro parse-kernel-ops (op)
  "Parse an op of the form (NAME ARGS &BODY BODY)"
  (destructuring-bind (name args &body body) op
    `(std/macs:named-lambda ,name ,args ,@body)))

(defmacro define-task-kernel (name ops accessors &body body)
  "Define a task kernel.

(define-task-kernel NAME (&key ARGS ACCESSORS)

The kernel should process all options and return a function - the
'kernel function'.

The kernel function is installed in worker threads by passing it to
SB-THREAD:MAKE-THREAD. It may accept a varying number of arguments
specified by ARGS.

ACCESSORS is a list of pandoric accessors which can be called on the
kernel via an ORACLE. 

This interface is experimental and subject to change."
  (declare (ignorable accessors ops))
  `(defun ,name ()
     ,@body
     (values)))

(defvar *task-kernel*
  (symbol-function
   (define-task-kernel default-task-kernel () ()
     "The default task kernel used to initialize the KERNEL slot of
task-pools.
"
     nil)))

;;; Supervisor
(defclass supervisor ()
  (scope)
  (:documentation "A class which provides a view of the work done within a specified
SCOPE.

This object should be used by operators to inspect 'runstreams'
performed in other threads, such as WORKERS in TASK-POOL.

Before using this object you should ensure the SCOPE is fully
initialized. Supervisors should be created at any point during the
lifetime of SCOPE, but never before and never after."))

;;; Worker
;; unix-getrusage  
;; 0,-1,-2
;; (multiple-value-list (sb-unix:unix-getrusage 0))
;; (setf sb-unix::*on-dangerous-wait* :error)
(defvar *default-worker-name* "worker")

(defclass worker ()
  ((thread :initform (%make-thread #.#1=(symbol-name (gensym "w")) t (make-semaphore :name #.#1#))
           :accessor worker-thread
           :initarg :thread)
   (function :type function :accessor worker-function :initarg :function)
   (arguments :type list :accessor worker-arguments :initarg :arguments)))

;;; Oracle           
(defstruct (oracle (:constructor %make-oracle (id thread)))
  (id 0 :type (unsigned-byte 32) :read-only t)
  (thread *current-thread* :read-only t))

(defun find-oracle (id)
  (find id *oracles* :test '= :key 'oracle-id))

(defun make-oracle (thread)
  (let* ((id (thread-os-tid thread)))
    (if-let ((found (find-oracle id)))
      (values id found)
      (let ((orc (%make-oracle id thread)))
        (push orc *oracles*)
        (values id orc)))))

;;; Proto

;; oracle
(defgeneric designate-oracle (host guest))
;; worker
(defgeneric make-worker (self &rest initargs &key &allow-other-keys)
  (:method ((self t) &key thread function arguments)
    (declare (ignore self))
    (apply #'make-instance 'worker
           `(,@(when thread `(:thread ,thread))
             ,@(when function `(:function ,function))
             ,@(when arguments `(:arguments ,arguments))))))
(defgeneric make-workers (self count &rest initargs &key &allow-other-keys)
  (:method ((self t) (count t) &key thread function arguments)
    (let ((ret))
      (dotimes (i count ret)
        (push (make-worker t :thread thread :function function :arguments arguments) ret)))))
(defgeneric push-worker (thread pool))
(defgeneric push-workers (threads pool))
(defgeneric worker-count (self &key &allow-other-keys))
(defgeneric delete-worker (worker pool &key &allow-other-keys))
(defgeneric pop-worker (pool))
(defgeneric make-worker-for (pool function &rest args)
  (:method ((pool null) (function function) &rest args)
    (declare (ignore pool))
    (make-worker t :function function :arguments args)))
(defgeneric make-workers-for (pool count function)
  (:method ((pool null) (count fixnum) (function function))
    (declare (ignore pool))
    (make-workers t count :function function)))
(defgeneric spawn-worker (pool)
  (:method ((pool null))
    (declare (ignore pool))
    (make-worker t :function (default-task-kernel))))
(defgeneric spawn-workers (pool count)
  (:method ((pool null) (count fixnum))
    (declare (ignore pool))
    (make-workers t count :function (default-task-kernel))))

;; job
(defgeneric make-job (self &key &allow-other-keys))
(defgeneric push-job (job pool))
(defgeneric pop-job (pool))
(defgeneric find-job (job pool &key &allow-other-keys))
(defgeneric delete-job (job pool &key &allow-other-keys))
(defgeneric run-job (self job))

;; task
(defgeneric push-task (task pool))
(defgeneric pop-task (pool))
(defgeneric delete-task (task pool &key &allow-other-keys))
(defgeneric make-task (&rest args))
(defgeneric run-task (self task))

;; task-result
(defgeneric push-task-result (task pool))
(defgeneric pop-task-result (pool))

;; stage
(defgeneric push-stage (stage pool))
(defgeneric pop-stage (pool))
(defgeneric delete-stage (stage pool &key &allow-other-keys))
(defgeneric run-stage (self stage))

;; task-pool
(defgeneric start-task-pool (pool))
(defgeneric pause-task-pool (pool))
(defgeneric init-task-pool (pool)
  (:method ((pool t))
    (setq *task-pool* (or pool (make-task-pool)))))
(defgeneric stop-task-pool (pool))
(defgeneric restart-task-pool (pool))

;;; Pool
(defstruct task-pool
  (oracle-id nil :type (or null (unsigned-byte 32)))
  (kernel #'default-task-kernel :type function)
  (jobs (make-queue :name "jobs"))
  (stages (make-array 0 :element-type 'stage :fill-pointer 0) :type (array stage *))
   ;; When open, indicates that the pool is fully initialized and workers
   ;; may make progress.
  (online (make-semaphore :name "online")
   :type semaphore)
  ;; TODO: test weak-vector here
  (workers (make-array 0 :element-type 'worker
                         :fill-pointer 0)
   :type (vector worker))
  (results (make-mailbox :name "results")))

(defmethod print-object ((self task-pool) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A :online ~A ~A:~A:~A ~A"
            (task-pool-oracle-id self)
            (task-pool-kernel self)
            (semaphore-count (task-pool-online self))
            (queue-count (task-pool-jobs self))
            (length (task-pool-stages self))
            (length (task-pool-workers self))
            (mailbox-count (task-pool-results self)))))

(defmethod designate-oracle ((self task-pool) (guest integer))
  (setf (task-pool-oracle-id self) (make-oracle (find-thread-by-id guest)))
  self)

(defmethod worker-count ((self task-pool) &key online)
  (if online
      (semaphore-count (task-pool-online self))
      (length (task-pool-workers self))))

(defmethod designate-oracle ((self task-pool) (guest thread))
  (designate-oracle self (make-oracle guest)))

(defmethod task-pool-oracle ((self task-pool))
  (oracle-thread (find-oracle (slot-value self 'oracle))))

(defmethod push-worker ((worker thread) (pool task-pool))
  (vector-push-extend worker (task-pool-workers pool)))

(defmethod push-workers ((threads list) (pool task-pool))
  (with-slots (workers) pool
    (dolist (w threads)
      (vector-push-extend w workers))))

(defmethod pop-worker ((pool task-pool))
  (vector-pop (task-pool-workers pool)))

(defmethod make-worker-for ((pool task-pool) function &rest args)
  (make-thread function :name *default-worker-name* :arguments args))

(defmethod make-workers-for ((pool task-pool) (count fixnum) function)
  (make-threads count function :name *default-worker-name*))

(defmethod spawn-worker ((pool task-pool))
  ;; (with-recursive-lock
  (push-worker (make-worker-for pool (task-pool-kernel pool)) pool))

(defmethod spawn-workers ((pool task-pool) (count fixnum))
  (push-workers (make-workers-for pool count (task-pool-kernel pool)) pool))

;;; Task
(defclass task ()
  ((state :initarg :state :accessor task-state)
   (object :initarg :object :accessor task-object))
  (:documentation "This object represents a single unit of work to be done by some
worker. Tasks are typically generated by an oracle, but workers may also be
granted the ability to create and distribute their own tasks. Once a task is
assigned, the 'owner', i.e. the worker that is assigned this task, may modify
the object and state. When the work associated with a task is complete, the
owner is responsible for indicating in the state slot the result of the
computation."))

(defmethod make-task (&rest args)
  (make-instance 'task :object args))

(defmethod print-object ((self task) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (task-object self))))

(defmethod push-task-result ((task task) (pool task-pool))
  (send-message (task-pool-results pool) task))

(defmethod run-task ((self thread) (task task)))

;;; Job
(defstruct (job (:constructor %make-job (tasks)))
  "A collection of tasks to be performed by worker threads."
  (tasks (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
   :type (array task *))
  (lock (make-mutex :name "job") :type mutex))

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

(defmethod run-job ((self task-pool) (job job))
  #+log (log:trace! "running remote job...")
  (push-job job self))

;;; Stage
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

;;; Macros
(defmacro with-task-pool ((sym pool &key count) &body body)
  `(let ((,sym ,(or pool std/thread::*task-pool*)))
     ,@(when count `((spawn-workers ,sym ,count)))
     ,@body))
