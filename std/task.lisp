;;; task.lisp --- Standard Task API

;; Tasks, Jobs, Plans, Oh My!

;;; Commentary:

;; Tasks are our preferred level of abstraction for dealing with 'units of
;; work'. The Threading API accessible via the THREAD-POOL class is not
;; natively aware of tasks since WORKERs only understand functions, but is
;; well-suited for executing them in worker threads nonetheless.

;; <2025-10-30 Thu> In addition to executing tasks as functions in worker
;; threads, this module aims to provide a TASK-POOL class.

;; Jobs are effectively a collection of tasks, and plans establish an order of
;; execution given an asynchronous context. Planners are to be used
;; pre-emptively (often at compile or macro-expand time) to optimize the
;; generation of efficient plans.

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

;;; Proto
(defgeneric task (self)
  (:documentation "Return the task associated with SELF."))
(defgeneric result (self)
  (:documentation "Return the result associated with SELF."))

(defgeneric tasks (self)
  (:documentation "Return the tasks associated with SELF.")
  (:method ((self list)) self))

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
(defkernel task ()
  ((state :initform nil :initarg :state :accessor state))
  (:documentation "This object represents a single unit of work to be done in a single thread by
some worker. Tasks are typically distributed from the pool, but workers may
also be granted the ability to create and distribute their own tasks, or be
assigned a single task to call repeatedly until told to stop. Once a task is
assigned, the 'owner', i.e. the worker that is assigned this task, may modify
the object. When the work associated with a task is complete, the owner is
responsible for indicating in the state slot the result of the computation."))

(defmethod print-object ((self task) stream)
  (print-unreadable-object (self stream :type t)
    (format stream ":state ~A" (state self))))

(defmethod taskp ((self task)) t)

(defun make-task (kernel &optional state)
  (let ((task (make-instance 'task :state state)))
    (set-funcallable-instance-function task kernel)
    task))

(defun run-task (worker task &optional (priority *task-priority*))
  "Run TASK in WORKER, which must be a task-worker."
  (push-priority-queue (tasks worker) task priority)
  (run-worker worker))

(defmethod run-object ((self task) &key (worker *worker*))
  (run-task worker self))

;;;; Scheduled Tasks
(defkernel scheduled-task (task)
  ((schedule :initarg :schedule :initform (get-universal-time) :accessor task-schedule))
  (:documentation "A task object with an associated schedule."))

(defmethod run-object ((self scheduled-task) &key time repeat absolute-p catch-up worker name)
  (sb-ext:schedule-timer
   (sb-ext:make-timer (state self) :thread worker :name name)
   time 
   :repeat-interval repeat :absolute-p absolute-p :catch-up catch-up))

;;; Job
(defkernel job (task)
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
  (setf (tasks worker) (make-priority-queue (length (tasks job)) :initial-contents (tasks job) :extend t))
  (run-worker worker))

(defmethod run-object ((self job) &key worker)
  (run-job worker self))

(defgeneric dependencies (self)
  (:method ((self t)) nil))

(defgeneric dependents (self)
  (:method ((self t)) nil))

;;; Async Tasks
(defkernel async-task (task) ()
  (:documentation "Asynchronous tasks compatible with the future/promise API in STD/ASYNC. Tasks
are scheduled and executed with the current *THREAD-POOL*."))

;;; Simple Tasks
(defkernel simple-task (task) ()
  (:documentation "Simple Tasks support sync/async variants of task objects."))

;;; Plan
;; One thing to note about ASDF:OPERATIONs is that ASDF does NOT distinguish
;; between multiple operations of the same class. All slots of all operations
;; must have :allocation :class. 
(defclass plan () ()
  (:documentation "Base class for plan objects."))

(defgeneric record-dependency (plan task context)
  (:documentation "Record a TASK on CONTEXT as a dependency in the current PLAN."))

(defgeneric task-done-p (task context)
  (:documentation "Return a boolean which is NIL if the action must be performed (again)."))

;;;; conditions

;;;; pressure
;; inspired by ASDF:FORCING
(defstruct pressure (performable t) parameters forced forced-not)

(defgeneric pressure (self)
  (:documentation "Return the pressure assigned to SELF.")
  (:method ((self null)) (make-pressure)))

(defgeneric task-forced-p (pressure task context)
  (:documentation "Return non-nil if TASK is being forced given PRESSURE and CONTEXT.")
  (:method ((pressure null) task context) nil))

(defgeneric task-prevented-p (pressure task context)
  (:documentation "Return non-nil if TASK is being forced to NOT happen given PRESSURE and
CONTEXT. Takes precedence over TASK-FORCED-P.")
  (:method ((pressure null) task context) nil))

;;;; plan traversal
(defclass plan-traversal (plan)
  ((pressure :initform (make-pressure) :initarg :pressure :reader pressure)))
(defclass simple-plan (plan-traversal)
  ((tasks :initform nil :accessor tasks))
  (:documentation "A simple plan is a list of tasks executed sequentially."))

;; No need to record a dependency to build a full graph, just accumulate nodes in order.
(defmethod record-dependency ((plan simple-plan) task component)
  (values))

(defgeneric mark-task-done (task context)
  (:documentation "Mark a TASK on CONTEXT as having just been done."))

;; compute-task-stamp?

;;;; status

;; status bits
(std:define-bitfield status-bits
  ;; 3 bits, same as ASDF
  (keep boolean)
  (done boolean)
  (need boolean))

(defstruct status
  (bits 0 :type status-bits)
  (stamp nil :type (or integer boolean))
  (level 0 :type fixnum)
  (index nil :type (or integer null)))

;; taskstamp?
(deftype timestamp () '(or real boolean))
(defun timestamp< (x y)
  (etypecase x
    ((eql t) (not (eql y t)))
    (real (etypecase y
            ((eql t) nil)
            (real (< x y))
            (null t)))
    (null nil)))
(defun timestamps< (list) (loop :for y :in list :for x = nil :then y :always (timestamp< x y)))
(defun timestamp*< (&rest list) (timestamps< list))
(defun timestamp<= (x y) (not (timestamp< y x)))
(defun earlier-timestamp (x y) (if (timestamp< x y) x y))
(defun timestamps-earliest (list) (reduce 'earlier-timestamp list :initial-value nil))
(defun earliest-timestamp (&rest list) (timestamps-earliest list))
(defun later-timestamp (x y) (if (timestamp< x y) y x))
(defun timestamps-latest (list) (reduce 'later-timestamp list :initial-value t))
(defun latest-timestamp (&rest list) (timestamps-latest list))
(define-modify-macro latest-timestamp-f (&rest timestamps) latest-timestamp)

(defun status-keep-p (status)
  (status-bits-keep (status-bits status)))
(defun status-done-p (status)
  (status-bits-done (status-bits status)))
(defun status-need-p (status)
  (status-bits-need (status-bits status)))

(defun merge-status (status1 status2) ;; status-and
  "Return the earliest status later than both status1 and status2"
  (make-status
   :bits (logand (status-bits status1) (status-bits status2))
   :stamp (latest-timestamp (status-stamp status1) (status-stamp status2))
   :level (min (status-level status1) (status-level status2))
   :index (or (status-index status1) (status-index status2))))

(defun mark-status-needed (status &optional (level 0))
  "Return the same status but with the need bit set, for the given level"
  (if (and (status-need-p status)
           (>= (status-level status) level))
      status
      (progn
        (make-status
         :bits (make-status-bits :keep (status-bits-keep (status-bits status)) 
                                 :done (status-bits-done (status-bits status))
                                 :need t)
         :level (max level (status-level status))
         :stamp (status-stamp status)
         :index (status-index status)))))

(defgeneric status (plan task context)
  (:documentation "Return the STATUS associated with TASK on CONTEXT in PLAN, or NIL if the task
wasn't visited yet."))

(defgeneric (setf status) (new plan task context)
  (:documentation "Sets the STATUS associated with TASK on CONTEXT in PLAN."))

;;;; planner
(defclass planner (scheduler) ())

;;;; planned tasks
;; propagators 
(defkernel planned-task (task) ())
(defkernel descending-task (planned-task) ())
(defkernel ascending-task (planned-task) ())
(defkernel sibling-task (planned-task) ())
(defkernel child-task (planned-task) ())
;; non-propagating
(defkernel solo-task (planned-task) ())

;;; Task Pool
(defclass task-pool (thread-pool)
  ((planner :initarg :planner :accessor planner))
  (:documentation "Task pools contain an additional PLANNER slot which stores
  an object responsible for generating CHANNELs and consuming PLANs."))
