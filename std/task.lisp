;;; task.lisp --- Standard Task API

;; 

;;; Code:
(in-package :std/task)

;;; Vars
(defvar *tasks*)
(defvar *jobs*)
(defvar *job*)
(defvar *stage*)
(defvar *task*)
(defvar *task-class* 'task)
(defvar *task-priority* nil)
(defvar *result* nil)

(define-condition task-error (thread-error) ()
  (:report (lambda (condition stream)
             (format stream "Unhandled task error in thread ~A" 
                     (thread-error-thread condition))))
  (:documentation "An error which occurs while processing a task."))

(defun task-error (thread)
  "Signal a TASK-ERROR associated with THREAD."
  (error 'task-error :thread thread))

;;; Kernel
(defmacro make-task-kernel (name args lock queue mailbox timeout &body body &environment env)
  (declare (ignorable env))
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

;;; Proto
(defgeneric task (self)
  (:documentation "Return the task associated with SELF."))
(defgeneric result (self)
  (:documentation "Return the result associated with SELF."))

(defgeneric tasks (self)
  (:documentation "Return the tasks associated with SELF."))
(defgeneric results (self)
  (:documentation "Return the results associated with SELF."))

(defgeneric jobp (self)
  (:method ((self t)) nil)
  (:documentation "Return Non-nil if SELF is a job."))
(defgeneric taskp (self)
  (:method ((self t)) nil)
  (:documentation "Return Non-nil if SELF is a task."))

;;; Task Worker
(defclass task-worker (worker)
  ((tasks :accessor tasks :initarg :tasks :type spin-queue))
  (:documentation "A Worker which stores a queue of TASKS."))

;;; Task Pool
(defclass task-pool (thread-pool)
  ((tasks :initform (if (boundp '*tasks*) *tasks*) :initarg :tasks :accessor tasks)
   ;; TODO: test weak-vector here
   (workers :initform (make-array 0 :element-type 'task-worker :adjustable t) :type (vector worker)
            :initarg :workers :accessor workers)
   (results :initform (make-mailbox :name "results") :accessor results :initarg :results))
  (:documentation "A thread-pool which maintains a dynamic list of TASKS."))

(defun task-pool-info (tp)
  "Return a plist of info about task-pool TP."
  (append
   (std/thread::thread-pool-info tp)
   (list
    :tasks (queue-count (tasks tp))
    :results (mailbox-count (results tp)))))

(defmethod print-object ((self task-pool) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~(~A ~^~)~{~s~^ ~}" (name self) (task-pool-info self))))

(defun kill-workers (pool)
  "Call FINISH-THREADS on task-pool's workers."
  (dotimes (i (length (workers pool)))
    (kill-worker (vector-pop (workers pool)))))

(defmethod designate-oracle ((self task-pool) (guest thread))
  (let ((id (make-oracle guest)))
    (setf (gethash id *oracle-table*)
          (vector-push-extend (sb-ext:make-weak-pointer self) (gethash id *oracle-table*)))))

(defmethod designate-oracle ((self task-pool) (guest (eql t)))
  (designate-oracle self *current-thread*))

(declaim (inline push-worker push-workers pop-worker))
(defun push-worker (worker pool)
  (vector-push-extend worker (workers pool)))

(defun push-workers (threads pool)
  "Push a list of THREADS to POOL."
  (with-slots (workers) pool
    (dolist (w threads)
      (vector-push-extend w workers))))

(defmethod pop-worker (pool)
  "Pop the next worker from POOL."
  (vector-pop (workers pool)))

(defun start-task-worker (pool index)
  "Start the TASK-WORKER at INDEX of POOL."
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

(defmethod taskp ((self task)) t)

(defun run-task (worker task)
  "Run TASK on WORKER."
  (push task (tasks worker))
  (run-worker worker))

(defmethod run-object ((self task) &key worker)
  (run-task worker self))

;;;; Scheduled Tasks
(defclass scheduled-task (task)
  ((schedule :initarg :schedule :initform (get-universal-time) :accessor task-schedule))
  (:documentation "A task object with an associated schedule."))

(defmethod run-object ((self scheduled-task) &key time repeat absolute-p catch-up worker name)
  (sb-ext:schedule-timer 
   (sb-ext:make-timer (task-state self) :thread worker :name name)
   time :repeat-interval repeat :absolute-p absolute-p :catch-up catch-up))

;;; Job
(defclass job (task)
  ((tasks :initform (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
          :type (array task *)
          :initarg :tasks
          :accessor tasks)
   (lock :initform (make-mutex :name "job") :type mutex
         :initarg :lock))
  (:documentation "A collection of tasks forming a single unit of work."))

(defgeneric jobs (self)
  (:documentation "Return the jobs associated with SELF."))
(defmethod jobp ((self job)) t)
(defmethod taskp ((self job)) t)
  
(declaim (inline make-job))
(defun make-job (&rest tasks)
  "Return a new job containing TASKS."
  (make-instance 'job
    :tasks (make-array (length tasks) 
                       :element-type 'task
                       :initial-contents tasks)))

(defmethod print-object ((self job) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A tasks" (length (tasks self)))))

(defun run-job (worker job)
  "Run JOB on WORKER."
  (setf (tasks worker) (coerce 'list (tasks job)))
  (run-worker worker))

(defmethod run-object ((self job) &key worker)
  (run-job worker self))

;;; Work Scope
(defclass work-scope ()
  ((tasks  :initform (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
           :initarg :tasks
           :accessor tasks
           :type (vector task))
   (lock :initform (make-mutex :name "work-scope") :initarg :lock :accessor work-scope-lock :type mutex))
  (:documentation "A scope of work containing TASKS and a LOCK."))

(defmethod print-object ((self work-scope) (stream stream))
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (tasks self))))

(defun make-task-pool (worker-count &key (name :default) (kernel *kernel*) 
                                         (task-class *task-class*) initial-task
                                         tasks
                                         alivep)
  "Make a new TASK-POOL with a worker capacity of WORKER-COUNT."
  (let ((*worker-class* 'task-worker))
    (let ((tp (make-thread-pool
               worker-count 
               :class 'task-pool
               :alivep alivep
               :name name
               :kernel kernel))
          (%tasks (or tasks worker-count)))
      (declare (task-pool tp))
      (setf (tasks tp)
            (make-queue
             :name "tasks"
             :initial-contents
             (make-array %tasks
                         :element-type task-class
                         :initial-element (or initial-task (make-instance task-class))))
            (results tp) (make-mailbox :name "results"))
      tp)))

;;; Macros
(defmacro with-task-pool ((sym &key (tasks (std/alien:num-cpus)) (workers (std/alien:num-cpus)) #+nil start)
                          &body body)
  "Eval BODY with SYM bound to a new TASK-POOL."
  `(let ((,sym (make-task-pool ,workers :tasks ,tasks)))
     ;; ,@(when start `((start-task-workers ,sym)))
     ,@body))
