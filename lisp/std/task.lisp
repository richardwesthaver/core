;;; task.lisp --- Standard Task API

;; 

;;; Code:
(in-package :std/task)

;;; Vars
(defvar *task-pool*)
(defvar *tasks* (make-queue :name "tasks"))
(defvar *jobs*)
(defvar *stages*)
(defvar *oracles* nil)
(defvar *task-oracles* nil)
(eval-when (:compile-toplevel)
  (defvar *task*)
  (defvar *task-result* nil))

(define-condition task-error (thread-error) ()
  (:report (lambda (condition stream)
             (format stream "Unhandled task error in thread ~A" 
                     (thread-error-thread condition)))))

(defun task-error (thread)
  (error 'task-error :thread thread))

;;; Kernel
(defmacro gen-task-kernel (name args lock queue mailbox timeout &body body)
  `(compile ,name 
            (lambda ,args 
              (wait-on-semaphore ,lock ,@(when timeout `((:timeout ,timeout))))
              (let ((*task* (dequeue ,queue)))
                (unwind-protect 
                     (handler-case (setf *task-result* (progn ,@body))
                       (error () (task-error *current-thread*)))
                  (send-message ,mailbox *task-result*)
                  (release-foreground))))))

(defmacro define-task-kernel (name (&key lock timeout mailbox queue) args &body body)
  "Define a task kernel.

(define-task-kernel NAME (&key ARGS ACCESSORS)

The kernel should process all options and return a function - the
'kernel function'.

The kernel function is installed in worker threads by passing it to
SB-THREAD:MAKE-THREAD. It may accept a varying number of arguments
specified by ARGS.

Within the BODY the variable *task* is bound to the result of (DEQUEUE QUEUE)
and *task-result* is bound to the return value of BODY.

This interface is experimental and subject to change."
  `(gen-task-kernel ,name ,args ,lock ,queue ,mailbox ,timeout
     ,@body))

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
  ((thread :initform (sb-thread::%make-thread #.#1=(symbol-name (gensym "w")) t (make-semaphore :name #.#1#))
           :accessor worker-thread
           :initarg :thread)
   (kernel :type function :accessor worker-kernel :initarg :kernel)
   (input :initform nil :accessor worker-input :initarg :input)))

(defvar *workers* (make-array 0 :element-type 'worker :adjustable t))

(declaim (inline kill-worker join-worker))
(defun start-worker (worker) 
  (sb-thread::start-thread (worker-thread worker) (worker-kernel worker) (worker-input worker)))
(defun kill-worker (worker) (kill-thread (worker-thread worker)))
(defun join-worker (worker) (join-thread (worker-thread worker)))

;;; Oracle           
(defstruct (oracle (:constructor %make-oracle (id thread)))
  (id 0 :type (unsigned-byte 32) :read-only t)
  (thread *current-thread* :read-only t))

(defun oracle-of-id (id)
  (find id *oracles* :test '= :key 'oracle-id))

(defun make-oracle (thread)
  (let ((id (thread-os-tid thread)))
    (if-let ((found (oracle-of-id id)))
      (values id found)
      (let ((orc (%make-oracle id thread)))
        (push orc *oracles*)
        (values id orc)))))

;;; Proto
;; oracle
(defgeneric designate-oracle (host guest))
;; worker
(defun make-worker (&key thread kernel input)
  (apply #'make-instance 'worker
         `(,@(when thread `(:thread ,thread))
           ,@(when kernel `(:kernel ,kernel))
           ,@(when input `(:input ,input)))))

(defgeneric make-workers (count &rest initargs &key &allow-other-keys)
  (:method ((count number) &key thread kernel input)
    (let ((ret))
      (dotimes (i count ret)
        (push (make-worker :thread thread :kernel kernel :input input) ret)))))

(defgeneric delete-worker (worker pool &key &allow-other-keys))
(defgeneric spawn-worker (pool worker))

;; job
(defgeneric make-job (self &key &allow-other-keys))
(defgeneric find-job (job pool &key &allow-other-keys))
(defgeneric run-job (self job))
(defgeneric run-jobs (self))
;; task
(defgeneric tasks (self))
(defgeneric run-task (self task))
(defgeneric run-tasks (self))
(defgeneric results (self))
;; stage
(defgeneric run-stage (self stage))
(defgeneric workers (self))

;;; Task Pool
(defstruct task-pool
  (kernel 'identity :type symbol)
  (tasks *tasks*)
  (lock (make-semaphore :name "online") :type semaphore)
  ;; TODO: test weak-vector here
  (workers (make-array 0 :element-type 'worker :fill-pointer 0) :type (vector worker))
  (results (make-mailbox :name "results")))

(defmethod tasks ((self task-pool)) (task-pool-tasks self))
(defmethod results ((self task-pool)) (task-pool-results self))
(defmethod workers ((self task-pool)) (task-pool-workers self))

(defmethod print-object ((self task-pool) (stream t))
  (print-unreadable-object (self stream :type t)
    (format stream "~A :workers ~A :tasks ~A/~A :results ~A"
            (task-pool-kernel self)
            (length (workers self))
            (queue-count (tasks self))
            (semaphore-count (task-pool-lock self))
            (mailbox-count (task-pool-results self)))))

(defun kill-workers (pool)
  "Call FINISH-THREADS on task-pool's workers."
  (dotimes (i (length (workers pool)))
    (kill-worker (vector-pop (workers pool)))))

(defun worker-count (task-pool &key online)
  (if online
      (semaphore-count (task-pool-lock task-pool))
      (length (task-pool-workers task-pool))))

(defmethod designate-oracle ((self task-pool) (guest thread))
  (designate-oracle self (make-oracle guest)))

(declaim (inline push-worker push-workers pop-worker))
(defun push-worker (worker pool)
  (vector-push-extend worker (task-pool-workers pool)))

(defun push-workers (threads pool)
  (with-slots (workers) pool
    (dolist (w threads)
      (vector-push-extend w workers))))

(defmethod pop-worker (pool)
  (vector-pop (task-pool-workers pool)))

(defun start-task-worker (pool index)
  ;; (with-recursive-lock
  (start-worker (aref (workers pool) index)))

(defun start-task-workers (pool)
  "Start all workers in the given task POOL."
  (loop for w across (workers pool)
        do (start-worker w)))

;;; Task
(defclass task ()
  ((state :initform nil :initarg :state :accessor task-state))
  (:documentation "This object represents a single unit of work to be done by some
worker. Tasks are typically generated by an oracle, but workers may also be
granted the ability to create and distribute their own tasks. Once a task is
assigned, the 'owner', i.e. the worker that is assigned this task, may modify
the object and state. When the work associated with a task is complete, the
owner is responsible for indicating in the state slot the result of the
computation."))

(defmethod print-object ((self task) stream)
  (print-unreadable-object (self stream :type t)
    (format stream ":state ~A" (task-state self))))

(defmethod run-task ((self thread) (task task)))

;;; Job
(defstruct (job (:constructor %make-job (tasks)))
  "A collection of tasks to be performed by worker threads."
  (tasks (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
   :type (array task *))
  (lock (make-mutex :name "job") :type mutex))

(defmethod tasks ((self job)) (job-tasks self))

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

(defmethod run-stage ((self thread) (stage stage)))

;;; Macros
(defmacro with-task-pool ((sym &key oracle (tasks 0) lock (workers 4) start kernel results) &body body)
  (unless lock (setf lock (make-semaphore :name "online" :count workers)))
  (unless results (setf results (make-mailbox :name "results")))
  `(let ((,sym (make-task-pool :lock ,lock :results ,results 
                               :tasks (make-queue 
                                       :name "tasks"
                                       :initial-contents
                                       (make-array ,tasks 
                                                   :element-type 'task 
                                                   :initial-element (make-instance 'task))))))
     ,@(if kernel `((setf (task-pool-kernel ,sym) ,kernel))
           `((setf (task-pool-kernel ,sym)
                   (gen-task-kernel (gensym "TASK-KERNEL") ()
                       (task-pool-lock ,sym) 
                       (tasks ,sym) 
                       (results ,sym)
                       nil))))
     (loop for i below ,workers
           do (push-worker (make-worker :kernel (task-pool-kernel ,sym)) ,sym))
     ,@(when oracle `((designate-oracle ,sym ,oracle)))
     ,@(when start `((start-task-workers ,sym)))
     ,@body))
