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
(defvar *task-priority* :low)

(define-condition task-error (thread-error) ()
  (:report (lambda (condition stream)
             (format stream "Unhandled task error in thread ~A" 
                     (thread-error-thread condition))))
  (:documentation "An error which occurs while processing a task."))

(defun task-error (thread)
  "Signal a TASK-ERROR associated with THREAD."
  (error 'task-error :thread thread))

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
  ((tasks :accessor tasks :initarg :tasks :type priority-queue))
  (:documentation "A Worker which stores an additional priority-queue of TASKS."))

;;; Task
(defclass task ()
  ((state :initform nil :initarg :state :accessor task-state))
  (:documentation "This object represents a single unit of work to be done by some
worker. Tasks are typically distributed from the pool, but workers may also be
granted the ability to create and distribute their own tasks. Once a task is
assigned, the 'owner', i.e. the worker that is assigned this task, may modify
the object. When the work associated with a task is complete, the owner is
responsible for indicating in the state slot the result of the computation."))

(defmethod print-object ((self task) stream)
  (print-unreadable-object (self stream :type t)
    (format stream ":state ~A" (task-state self))))

(defmethod taskp ((self task)) t)

(defun run-task (worker task)
  "Run TASK on WORKER."
  (push-priority-queue (tasks worker) task *task-priority*)
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
  (setf (tasks worker) (make-priority-queue (length (tasks job)) :initial-contents (tasks job)))
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

(defmethod print-object ((self work-scope) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (tasks self))))

;; RESEARCH 2025-07-26: 
;;; Task Scheduler?
