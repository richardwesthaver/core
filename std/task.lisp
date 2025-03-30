;;; task.lisp --- Standard Task API

;; 

;;; Code:
(in-package :std/task)

;;; Vars
(defvar *task-pool*)
(defvar *tasks* (make-queue :name "tasks"))
(defvar *jobs*)
(defvar *stages*)
(sb-ext:defglobal *worker-threads* nil)
(sb-ext:defglobal *supervisor-threads* nil)
(sb-ext:defglobal *oracle-table* (make-hash-table))
(defvar *task*)
(defvar *result* nil)

(define-condition task-error (thread-error) ()
  (:report (lambda (condition stream)
             (format stream "Unhandled task error in thread ~A" 
                     (thread-error-thread condition)))))

(defun task-error (thread)
  (error 'task-error :thread thread))

;;; Kernel
(defmacro make-task-kernel (name args lock queue mailbox timeout &body body)
  `(compile ',name 
            (lambda ,args 
              (wait-on-semaphore ,lock ,@(when timeout `((:timeout ,timeout))))
              (let ((*task* (dequeue ,queue)))
                (unwind-protect 
                     (handler-case (setf *result* (progn ,@body))
                       (error () (task-error *current-thread*)))
                  (send-message ,mailbox *result*)
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
and *result* is bound to the return value of BODY.

This interface is experimental and subject to change."
  `(make-task-kernel ,name ,args 
       ,(if lock lock '(make-semaphore))
       ,(if queue queue '(make-queue))
       ,(if mailbox mailbox '(make-mailbox))
       ,timeout
     ,@body))

(defun make-ephemeral-thread (name)
    (sb-thread::%make-thread name t (make-semaphore :name name)))

;;; Proto
(defgeneric designate-oracle (host guest))
(defgeneric assign-supervisor (worker supervisor))

(defgeneric make-workers (count &rest initargs &key &allow-other-keys)
  (:method ((count number) &key thread function tasks bind (return-type 'vector))
    (let ((ret))
      (dotimes (i count)
        (push (make-worker :thread thread :function function :tasks tasks :bind bind) ret))
      (if return-type (coerce ret return-type) ret))))

(defgeneric task (self))
(defgeneric result (self))

(defgeneric tasks (self))
(defgeneric results (self))

(defgeneric status (self &key &allow-other-keys))

;;; Supervisor
(defclass supervisor ()
  ((thread :initform (make-ephemeral-thread (symbol-name (gensym "supervisor"))) :accessor supervisor-thread)
   (domain)
   (scope))
  (:documentation "Supervisors are threads which are responsible for a set of worker threads
within their DOMAIN and SCOPE."))

(defmethod initialize-instance :after ((self supervisor) &key &allow-other-keys)
  (push (supervisor-thread self) *supervisor-threads*))

;;; Worker
;; unix-getrusage  
;; 0,-1,-2
;; (multiple-value-list (sb-unix:unix-getrusage 0))
;; (setf sb-unix::*on-dangerous-wait* :error)

;; TODO 2024-10-03: with-cas-lock?
(defclass worker ()
  ((thread :initform (make-ephemeral-thread (symbol-name (gensym "worker")))
           :accessor worker-thread
           :initarg :thread)
   (bind :type list :accessor worker-bind :initarg :bind)
   (function :type function :accessor worker-function :initarg :function)
   (tasks :initform nil :accessor tasks :initarg :tasks)))

(defmethod initialize-instance :after ((self worker) &key &allow-other-keys)
  (push (worker-thread self) *worker-threads*))

(defun make-worker (&key thread function tasks bind)
  (apply #'make-instance 'worker
         `(,@(when thread `(:thread ,thread))
           ,@(when function `(:function ,function))
           ,@(when tasks `(:tasks ,tasks))
           ,@(when bind `(:bind ,bind)))))

;; TODO 2024-10-03: pause/resume
(declaim (inline kill-worker join-worker start-worker run-worker))
(defun start-worker (worker) 
  (sb-thread::start-thread (worker-thread worker) (worker-function worker) (tasks worker)))

(defun run-worker (worker &key tasks bind wait)
  (when tasks
    (setf (tasks worker) tasks))
  (when bind
    (setf (worker-bind worker) bind))
  (start-worker worker)
  (if wait (join-worker worker)
      worker))

(defmethod run-object ((self worker) &key)
  (run-worker self))

(defun run-with-worker (worker object &key wait)
  (run-worker worker :tasks (list object) :wait wait))

(defun kill-worker (worker) 
  (declare (worker worker))
  (let ((th (worker-thread worker)))
    (unwind-protect (kill-thread th)
      (deletef *worker-threads* th))))

(defun join-worker (worker)
  (declare (worker worker))
  (let ((th (worker-thread worker)))
    (unwind-protect (join-thread th)
      (deletef *worker-threads* th))))

;;; Oracle
(defstruct (oracle (:constructor %make-oracle (id thread)))
  "Oracles provide a tagged view into some threaded scope of work."
  (id 0 :type (unsigned-byte 32) :read-only t)
  (thread *current-thread* :read-only t))

(defun oracle-of-id (id)
  (gethash id *oracle-table*))

(defun make-oracle (thread)
  (let ((id (thread-os-tid thread)))
    (if-let ((found (oracle-of-id id)))
      (values id found)
      (let ((orc (%make-oracle id thread)))
        (setf (gethash id *oracle-table*) (make-array 0 :adjustable t))
        (values id orc)))))

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
  (let ((id (make-oracle guest)))
    (setf (gethash id *oracle-table*)
          (pushnew (sb-ext:make-weak-pointer self) (gethash id *oracle-table*)))))

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
worker. Tasks are typically distributed from the task-pool, but workers may
also be granted the ability to create and distribute their own tasks. Once a
task is assigned, the 'owner', i.e. the worker that is assigned this task, may
modify the object. When the work associated with a task is complete, the owner
is responsible for indicating in the state slot the result of the computation."))

(defmethod print-object ((self task) stream)
  (print-unreadable-object (self stream :type t)
    (format stream ":state ~A" (task-state self))))

(defun run-task (worker task)
  (run-worker worker :tasks (list task)))

(defmethod run-object ((self task) &key worker)
  (run-task worker self))

;;;; Scheduled Tasks
(defgeneric schedule (self))

(defclass scheduled-task (task)
  ((schedule :initarg :schedule :initform nil :accessor schedule)))

(defmethod run-object ((self scheduled-task) &key time repeat absolute catch-up)
  (sb-ext:schedule-timer (task-state self) time :repeat-interval repeat :absolute-p absolute :catch-up catch-up))

;;; Job
(defclass job (task)
  ((tasks :initform (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
          :type (array task *)
          :initarg :tasks
          :accessor tasks)
   (lock :initform (make-mutex :name "job") :type mutex
         :initarg :lock))
  (:documentation "A collection of tasks forming a single unit of work."))

(defgeneric jobs (self))

(declaim (inline make-job))
(defun make-job (&rest tasks)
  (make-instance 'job
    :tasks (make-array (length tasks) 
                       :element-type 'task
                       :initial-contents tasks)))

(defmethod print-object ((self job) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A tasks" (length (tasks self)))))

(defun run-job (worker job)
  (run-worker worker :tasks (coerce 'list (tasks job))))

(defmethod run-object ((self job) &key worker)
  (run-job worker self))

;;; Work Scope
(defclass work-scope ()
  ((tasks  :initform (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
           :initarg :tasks
           :accessor tasks
           :type (vector task))
   (lock :initform (make-mutex :name "work-scope") :initarg :lock :accessor work-scope-lock :type mutex)))

(defmethod print-object ((self work-scope) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (tasks self))))

;;; Macros
(defmacro with-task-pool ((sym &key oracle (tasks 0) lock (workers 4) start function kernel results) &body body)
  (unless lock (setf lock (make-semaphore :name "online" :count workers)))
  (unless results (setf results (make-mailbox :name "results")))
  `(let ((,sym (make-task-pool :lock ,lock :results ,results
                               :tasks (make-queue 
                                       :name "tasks"
                                       :initial-contents
                                       (make-array ,tasks 
                                                   :element-type 'task 
                                                   :initial-element (make-instance 'task))))))
     (setf *worker-threads* nil
           *task-pool* ,sym)
     ,@(if kernel `((setf (task-pool-kernel ,sym) ,kernel))
           `((setf (task-pool-kernel ,sym)
                   (make-task-kernel ,(gensym "TASK-KERNEL") ()
                       (task-pool-lock ,sym) 
                       (tasks ,sym) 
                       (results ,sym)
                       nil))))
     (loop for i below ,workers
           do (push-worker (make-worker :function ,function) ,sym))
     ,@(when oracle `((designate-oracle ,sym ,oracle)))
     ,@(when start `((start-task-workers ,sym)))
     ,@body))
