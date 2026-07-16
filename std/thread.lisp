;;; threads.lisp --- Multi-thread utilities

;; Threading Macros

;;; Commentary:

;; mostly yoinked from sb-thread and lparallel

#|
;; unix-getrusage  
;; 0,-1,-2
;; (multiple-value-list (sb-unix:unix-getrusage 0))
;; (setf sb-unix::*on-dangerous-wait* :error)

;; TODO 2024-10-03: with-cas-lock?
|#
;;; Code:
(in-package :std/thread)

;;; Types
(deftype worker-kernel-function ()
  "A function which is suitable as a kernel for KIND workers."
  `(function (&optional t) t))
(deftype pool-kernel-function ()
  "A function which is suitable as a thread-pool kernel. Accepts a single keyword argument."
  `(function (keyword) (values)))

;;; Vars
(defvar *default-special-bindings* nil
  "This variable holds an alist associating special variable symbols
  to forms to evaluate. Special variables named in this list will
  be locally bound in the new thread before it begins executing user code.

  This variable may be rebound around calls to MAKE-THREAD to
  add/alter default bindings. The effect of mutating this list is
  undefined, but earlier forms take precedence over later forms for
  the same symbol, so defaults may be overridden by consing to the
  head of the list.")

(defvar *worker-class* 'worker
  "The default WORKER class used to initialize THREAD-POOLs.")
(defvar *worker* nil
  "The current WORKER or nil.")
(defvar *work-priority* :default
  "The default priority assigned to new work.")
(defvar *scheduler-class* 'biased-scheduler
  "The default class of the scheduler used in THREAD-POOLs.")
(defvar *thread-pool* nil
  "The current THREAD-POOL or nil.")
;; on core-i7 3.4ghz, a single spin takes ~ 2.5 microseconds.
(defvar *default-spin-count* 1000
  "Default value of the 'spin-count' argument to MAKE-THREAD-POOL.")

(defvar *debug-threads-p* nil
  "When non-nil the debugger is invoked when an error goes unhandled in a
threaded context.")

(defvar *lisp-exiting-p* nil
  "True if the Lisp process is exiting - used for skipping auto-replacement of
killed workers during shutdown.")

(declaim (pool-kernel-function %pool))
(definline %pool (state &optional (self *thread-pool*))
  "Default pool-kernel-function, user is responsible for ensuring *THREAD-POOL*
is bound to the correct target THREAD-POOL before calling."
  (declare (optimize (speed 3) (safety 0)))
  (ecase state
    (:start (start-thread-pool self))
    (:stop (stop-thread-pool self))
    (:reset (reset-thread-pool self))
    (:shutdown (stop-thread-pool self :wait t))
    (:kill (kill-thread-pool self)))
  (values))

(defparameter *pool-kernel* (make-kernel #'%pool)
  "A function which drives THREAD-POOLs.")

(declaim (worker-kernel-function %work))

(definline %work (&optional work)
  "Default worker-kernel-function."
  (declare (optimize (speed 3) (safety 0)))
  (let ((work (or work (when-let ((w (work *worker*))) (pop-spin-queue w)))))
    (typecase work
      (null)
      (cons (apply (the function (car work)) (cdr work)))
      (function (funcall work))
      (t work))))

(defparameter *worker-kernel* (make-kernel #'%work)
  "A kernel which drives WORKERs.")

;;; Globals
(sb-ext:defglobal *super-threads* nil
    "List of threads with supervisor privileges.")
(sb-ext:defglobal *oracle-table* (make-hash-table)
    "Hashtable containining (ID . ORACLE-SCOPE).")
(sb-ext:defglobal *thread-pool-table* (make-hash-table)
    "Hashtable containing (NAME . THREAD-POOL).")

;;; Conditions
(defvar *error-workers* nil
  "Track debugger popups in order to kill them.")

(defvar *error-workers-lock* (make-mutex :name "error workers")
  "Lock for *ERROR-WORKERS*.")

(defconstant +work-tag+ '%work)

(defvar *debugger-error* nil
  "Track the error inside the debugger for the `transfer-error' restart.")

(defvar *handler-active-p* nil
  "Non-nil when handlers have been established via `call-with-work-handler'.")

(defun unwrap-result (result)
  "In `receive-result', this is called on the stored work result. The
user receives the return value of this function."
  (typecase result
    (wrapped-error
     ;; A `wrapped-error' signals an error upon being unwrapped.
     (error (wrapped-condition-value result)))
    (t
     ;; Most objects unwrap to themselves.
     result)))

(defmacro work-handler-bind (clauses &body body)
  "Like `handler-bind' but handles conditions signaled inside work
that was created in `body'."
  (let ((forms (loop for clause in clauses
                     for (name fn . more) = clause
                     do (unless (and name (symbolp name) fn (not more))
                          (error "Ill-formed binding in `work-handler-bind': ~a"
                                 clause))
                     collect `(cons ',name ,fn))))
    `(let ((*handlers* (list* ,@forms *handlers*)))
       ,@body)))

(defun transfer-error-restart (&optional (err *debugger-error*))
  (when err
    (throw '#.+work-tag+ (wrap-error err))))

(defun call-with-tracked-error (condition body-fn)
  (when *worker*
    (with-mutex (*error-workers-lock*)
      (push *worker* *error-workers*)))
  (unwind-protect
       (let ((*debugger-error* condition))
         (funcall body-fn))
    (when *worker*
      (with-mutex (*error-workers-lock*)
        (setf *error-workers*
              (delete *worker* *error-workers*))))))

(defmacro with-tracked-error (condition &body body)
  `(call-with-tracked-error ,condition (lambda () ,@body)))

(defun make-debugger-hook ()
  "Record `*debugger-error*' for the `transfer-error' restart."
  (if *debugger-hook*
      (let ((previous-hook *debugger-hook*))
        (lambda (condition self)
          (with-tracked-error condition
            (funcall previous-hook condition self))))
      (lambda (condition self)
        (declare (ignore self))
        (with-tracked-error condition
          (invoke-debugger condition)))))

(defmacro with-work-context (&body body)
  "Eval BODY in a context where throw to +WORK-TAG+ will be caught."
  `(catch '#.+work-tag+ ,@body))

(defun %call-with-work-handler (fn)
  "Call FN with worker conditions handled."
  (declare (function fn))
  (let ((*handler-active-p* t)
        (*debugger-hook* (make-debugger-hook)))
    (handler-bind ((condition #'condition-handler))
      (restart-bind ((transfer-error #'transfer-error-restart
                                     :report-function #'transfer-error-report))
        (funcall fn)))))

(defun call-with-work-handler (fn)
  "Call FN in a worker context with conditions handled."
  (declare (function fn))
  (with-work-context
    (if *handler-active-p*
        (funcall fn)
        (%call-with-work-handler fn))))

(define-condition worker-killed-error (error) ()
  (:report
   "The worker was killed.")
  (:documentation
   "Error signaled when attempting to obtain a result from a killed worker."))

(define-condition no-thread-pool-error () ()
  (:report
   "invalid *THREAD-POOL*")
  (:documentation
   "Error signaled when a kernel object is invalid."))

;;; Utils
(defun timer-p (self)
  (typep self 'sb-ext:timer))

(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if repeat is non-nil.
SECS and REPEAT may be reals.
The action is to call FUNCTION with arguments ARGS."
  (check-type secs (real 0 *))
  (check-type repeat (or null (real 0 *)))
  (check-type function (or function symbol))
  (let ((timer (sb-ext:make-timer (lambda () (apply function args)) :thread t)))
    (sb-ext:schedule-timer timer secs :repeat-interval repeat)
    timer))

(defun timer-expired-p (timer now &optional (delta 0.0d0))
  (assert (sb-impl::%timer-expire-time timer) ((sb-impl::%timer-expire-time timer))
          "Timer ~A must have an expiry time set." timer)
  (let ((compare-time (+ now delta)))
    (> compare-time (sb-impl::%timer-expire-time timer))))

(defun thread-support-p () 
  "Return Non-nil if threads are supported on this system. (:THREAD-SUPPORT feature)"
  (member :thread-support *features*))

(eval-always
  (defun print-top-level (msg)
    "Print MSG to the top-level *STANDARD-OUTPUT*."
    (let ((*standard-output* *standard-output*))
      (sb-thread:make-thread
       (lambda ()
         (format *standard-output* "~A" msg)))
      nil)))

(defun println-top-level (msg)
  "Print MSG to the top-level *STANDARD-OUTPUT* followed by a newline."
  (let ((*standard-output* *standard-output*))
    (sb-thread:make-thread
     (lambda ()
       (format *standard-output* "~A~%" msg)))
    nil))

(defun find-thread-by-id (id)
  "Search for thread by ID which must be an u64. On success returns the thread itself or nil."
  (find id (sb-thread::list-all-threads) :test '= :key 'thread-os-tid))

(defun find-thread (name)
  "Find a thread by name."
  (find name (sb-thread::list-all-threads) :test 'equal :key 'thread-name))

(defun thread-key-list ()
  "Return AVLNODE-KEYs associated with threads in *ALL-THREADS*."
  (sb-thread::avltree-filter #'sb-thread::avlnode-key sb-thread::*all-threads*))

(defun thread-id-list ()
  "Return the THREAD-OS-TID associated with thread in *ALL-THREADS*."
  (sb-thread::avltree-filter (lambda (th) (thread-os-tid (sb-thread::avlnode-data th))) sb-thread::*all-threads*))

(defun thread-count ()
  "Return the current count of threads in *ALL-THREADS*."
  (sb-thread::avl-count sb-thread::*all-threads*))

(defun make-threads (n thunk &key (name "thread"))
  "Make N number of threads which each eval THUNK."
  (declare (type fixnum n))
  (loop for i below n
        collect (make-thread thunk :name (format nil "~A-~D" name i))))

(defun make-ephemeral-thread (name)
  "Make a new 'ephemeral' thread called NAME."
  (sb-thread::%make-thread name t (make-semaphore :name name)))

(defgeneric designate-oracle (host guest)
  (:documentation "Designate an oracle GUEST for HOST."))

(defgeneric assign-supervisor (worker supervisor)
  (:documentation "Assign a SUPERVISOR for WORKER."))

;;; Threads
(defmacro with-thread ((&key bindings name) &body body)
  "Eval BODY in a new thread with optional BINDINGS and NAME."
  `(with-default-special-bindings ,bindings
     (make-thread (lambda () ,@body)
		  ,@(when name `(:name ,name)))))

(declaim (inline parse-lambda-list-names))
(defun parse-lambda-list-names (ll)
  (multiple-value-bind (idx _ args) (sb-int:parse-lambda-list ll)
    (declare (ignore idx _))
    (loop for a in args
	  collect
	     (etypecase a
	       (atom a)
	       (cons (car a))))))

(defmacro with-threads ((i n &key return bindings args name) &body body)
  "Eval BODY N times in a function with I bound to a new thread. Optional
keywords modify the bindings in effect."
  `(with-default-special-bindings ,bindings
     (dotimes (,i ,n ,@(when return (list return)))
       (make-thread (lambda (,@args) ,@body)
                    ,@(when name `(:name (symbolicate ,name i)))))))

(defun finish-threads (&rest threads)
  "Finish THREADS, attempting to join them, else calling TERMINATE-THREAD."
  (let ((threads (flatten threads)))
    (unwind-protect
	 (mapc #'join-thread threads)
      (dolist (thread threads)
	(when (thread-alive-p thread)
	  (terminate-thread thread))))))

(defun timed-join-thread (thread timeout)
  "Join THREAD waiting at most TIMEOUT seconds."
  (declare (type thread thread) (type float timeout))
  (handler-case (sb-sys:with-deadline (:seconds timeout)
		  (join-thread thread :default :aborted))
    (sb-ext:timeout ()
      :timeout)))

(defun hang ()
  "Attempt to join the current thread, causing it to hang. You should never call this."
  (join-thread *current-thread*))

(defun kill-thread (thread)
  "Kill THREAD, ignoring all errors which may occur."
  (when (thread-alive-p thread)
    (ignore-errors
     (terminate-thread thread))))

;; (sb-vm::primitive-object-slots (sb-vm::primitive-object 'sb-vm::thread))
;; (defun init-session (&optional (thread *current-thread*)) (sb-thread::new-session thread))

;; (sb-thread::with-progressive-timeout (timet :seconds 4) (dotimes (i 4000) (print (timet))))

;; (describe sb-thread::*session*)

;; make-listener-thread

;; with-progressive-timeout

;; (definline all-threads-sap ()
;;   (sb-vm::extern-alien "all_threads" sb-vm::system-area-pointer))

(definline wait-for-threads (threads)
  (map 'list (lambda (thread) (sb-thread:join-thread thread :default nil)) threads))

(defun process-all-interrupts (&optional (thread sb-thread:*current-thread*))
  (sb-ext:wait-for (null (sb-thread::thread-interruptions thread))))

;;;; Thread Wrappers
;; BORDEAUX-THREADS version
(defun condition-wait* (cvar lock &key timeout)
  (let ((success (condition-wait cvar lock :timeout timeout)))
    (when (not success)
      (grab-mutex lock))
    success))

(sb-ext:defglobal .known-threads-lock. (make-mutex :name "known-threads-lock"))
(sb-ext:defglobal .known-threads. (make-hash-table :weakness :key))

(defun %get-thread-wrapper (native-thread)
  (multiple-value-bind (thread presentp)
      (with-mutex (.known-threads-lock.)
	(gethash native-thread .known-threads.))
    (if presentp
	thread
	(error "Thread wrapper is supposed to exist for ~S"
	       native-thread))))

(defun (setf thread-wrapper) (thread native-thread)
  (with-mutex (.known-threads-lock.)
    (setf (gethash native-thread .known-threads.) thread)))

(defun remove-thread-wrapper (native-thread)
  (with-mutex (.known-threads-lock.)
    (remhash native-thread .known-threads.)))

;; Forms are evaluated in the new thread or in the calling thread?
(macrolet
    ((defbindings (name docstring &body initforms)
	 (check-type docstring string)
       `(std/macs:define-constant ,name
	    (list
	     ,@(loop for (special form) in initforms
		     collect `(cons ',special ',form)))
	  :test #'equal
	  :documentation ,docstring)))
  (defbindings +standard-io-bindings+
      "Standard bindings of printer/reader control variables as per
CL:WITH-STANDARD-IO-SYNTAX. Forms are evaluated in the calling thread."
    (*package*                   (find-package :common-lisp-user))
    (*print-array*               t)
    (*print-base*                10)
    (*print-case*                :upcase)
    (*print-circle*              nil)
    (*print-escape*              t)
    (*print-gensym*              t)
    (*print-length*              nil)
    (*print-level*               nil)
    (*print-lines*               nil)
    (*print-miser-width*         nil)
    (*print-pprint-dispatch*     (copy-pprint-dispatch nil))
    (*print-pretty*              nil)
    (*print-radix*               nil)
    (*print-readably*            t)
    (*print-right-margin*        nil)
    (*random-state*              (make-random-state t))
    (*read-base*                 10)
    (*read-default-float-format* 'double-float)
    (*read-eval*                 nil)
    (*read-suppress*             nil)
    (sb-ext:*suppress-print-errors* nil)
    (sb-ext:*print-vector-length* nil)
    (*readtable*                 (copy-readtable nil))))

(defun compute-special-bindings (bindings)
  (remove-duplicates (append bindings +standard-io-bindings+)
		     :from-end t :key #'car))

;;; Channel
(defstruct (channel (:constructor %make-channel))
  (queue (make-queue) :type queue)
  (pool *thread-pool*))

(defaccessor queue ((self channel)) (channel-queue self))

(definline make-channel (&key (pool *thread-pool*) capacity)
  (%make-channel :queue (make-queue :capacity capacity) :pool pool))

;; (defmacro defchannel ())

;;; Limiter
(defclass thread-limiter ()
  ((limiter-lock :accessor limiter-lock :initarg :limiter-lock)
   (limiter-count :accessor limiter-count :initarg :limiter-count :type fixnum)))

(defun initial-limiter-count (thread-count) (+ thread-count 1))

;;; Kill
(defconstant +worker-suicide-tag+ 'worker-suicide-tag)

(defun kill-errors ()
  (let ((suicide nil))
    (with-mutex (*error-workers-lock*)
      (dolist (worker *error-workers*)
	(if (and *worker* (eq worker *worker*))
	    (setf suicide t)
	    ;; user could possibly (though unlikely) destroy the
	    ;; thread simultaneously, so ignore double-destroy error
	    (ignore-errors (terminate-thread (worker-thread worker)))))
      (when suicide
	(assert (eq (worker-thread *worker*) *current-thread*))
	(throw '#.+worker-suicide-tag+ nil)))))

(defun kill-errors-report (stream)
  (format stream "Kill errors in workers (remove debugger instances)."))

(eval-always
  (defvar *worker-restarts* '((kill-errors #'kill-errors :report-function #'kill-errors-report))
    "A list of restarts available in the body of a WITH-WORKER-RESTARTS form."))

(defmacro with-worker-restarts (&body body)
  "Eval BODY in a worker context with restarts and a catch for
+WORKER-SUICIDE-TAG+. See variable *WORKER-RESTARTS*."
  `(catch +worker-suicide-tag+
     (restart-bind ,*worker-restarts*
       ,@body)))

;;; Worker
(defvar *default-worker-tx-capacity* 8)
(defclass worker-status ()
  ((%rx :initform (sb-concurrency:make-gate))
   (%tx :initform (make-queue :capacity *default-worker-tx-capacity*))))

(defclass worker (worker-status)
  ((name :allocation :class :initarg :name :initform "worker")
   (thread :accessor worker-thread
	   :initarg :thread)
   ;; REVIEW 2026-02-16: consider making this class-allocated
   (kernel :initform *worker-kernel* :accessor kernel)
   (work :accessor work :type spin-queue :initarg :work)
   (idx :type array-index :initarg :idx :accessor idx)
   (bind :type list :accessor worker-bind :initarg :bind :initform *default-special-bindings* :accessor bind)))

(defmethod name ((self worker)) (thread-name (worker-thread self)))

(defmethod initialize-instance :after ((self worker) &key thread &allow-other-keys)
  (unless thread (setf (worker-thread self) (make-ephemeral-thread (symbol-name (gensym (slot-value self 'name)))))))

(defmethod print-object ((self worker) stream)
  (let* ((thread (worker-thread self))
         (state (cond ((thread-alive-p thread) :running)
                       ;; don't call JOIN-THREAD, just read the result if ALIVE-P is NIL
                       ((listp (sb-thread::thread-result thread)) 
                        (cons :finished (sb-thread::thread-result thread)))
                       (t :aborted)))
         (*print-array* nil)
         ;; Don't want to see 10,000 strings or something
         (*print-length* 2)
         (*print-level* 4))
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~@[~A ~]~X~@[ ~S~]" (name self) (idx self) state))))

(defun make-worker* (&key thread kernel bind index)
  (apply #'make-instance *worker-class*
	 `(,@(when thread `(:thread ,thread))
           :idx ,(or index (random 1024))
	   ,@(when kernel `(:kernel ,kernel))
	   ,@(when bind `(:bind ,bind)))))

(defmacro with-default-special-bindings (bindings &body body)
  `(let ((*default-special-bindings* ,bindings))
     ,@body))

;; TODO 2024-10-03: pause/resume 

(declaim (inline kill-worker join-worker start-worker run-worker))
(defun start-worker (worker &rest args)
  (with-default-special-bindings (worker-bind worker)
    (sb-thread::start-thread (worker-thread worker) (kernel worker) args)))

;; TODO 2025-11-03: should we abuse the initialization protocol more? want a
;; queue of 'next worker ids' to submit work to in a reverse-priority fashion.

(defun run-worker (worker &key bind wait)
  (when bind
    (setf (worker-bind worker) bind))
  (start-worker worker)
  (if wait (join-worker worker)
      worker))

(defmethod exec ((self worker))
  (run-worker self))

(defmethod run-object ((self worker) &key)
  (exec self))

(defun run-with-worker (worker object &key wait)
  (run-worker worker :bind object :wait wait))

(definline thread= (a b) (and (eql (sb-thread:thread-alive-p a) (sb-thread:thread-alive-p b) )
                              (= (thread-os-tid a) (thread-os-tid b))))

(definline worker= (a b) 
  (and a b
       (or
        (= (idx a) (idx b))
        (thread= (worker-thread a) (worker-thread b)))))

(defun kill-worker (worker) 
  (declare (worker worker))
  (let ((th (worker-thread worker)))
    (unless (null th)
      (kill-thread th))))

(defun join-worker (worker)
  (declare (worker worker))
  (let ((th (worker-thread worker)))
    (join-thread th)))

;; called from pool
(defun send-worker-start (worker)
  (assert (sb-concurrency:open-gate (slot-value worker '%rx)) nil "Failed to start worker ~A" worker))

(defun receive-worker-status (worker)
  (ecase (pop-queue (slot-value worker '%tx))
    (:ok :ok)
    (:exit :exit)
    (:error (error 'kernel-init-error))))

;; called from worker
(defun receive-worker-start (worker)
  ;; (print-top-level (format nil "worker ~A starting...~%" (worker-index worker)))
  (let ((gate (slot-value worker '%rx)))
    (sb-concurrency:wait-on-gate gate)))
    ;; (assert (sb-concurrency:close-gate gate) nil "Worker hijacked? ~A" worker))

(defun send-worker-status (worker status)
  (check-type status (member :ok :error :exit))
  ;; (print-top-level (format nil "worker ~A status: ~A~%" (worker-index worker) status))
  (push-queue status (slot-value worker '%tx)))

(defun notify-exit (worker)
  ;; (print-top-level (format nil "worker ~A exiting...~%" (worker-index worker)))
  (send-worker-status worker :exit)
  (sb-concurrency:close-gate (slot-value worker '%rx)))

(defun wait-for-worker (worker)
  ;; (std/print:mumble "waiting on worker ~A...~%" (worker-index worker))
  (unless (null (thread-alive-p (worker-thread worker)))
    (assert (eql :exit (receive-worker-status worker)))))

;;;; Worker Protocol
(defgeneric workers (self))
(defgeneric work (self)
  (:method ((self null)) nil))
(defgeneric run-thread (self thunk &key name &allow-other-keys))

(defun make-workers (count &key thread kernel bind (return-type 'vector))
  (let ((ret))
    (dotimes (i count)
      (push (make-worker* :thread thread :kernel kernel :bind bind) ret))
    (if return-type (coerce ret return-type) ret)))

;;; Scheduler
(defclass scheduler ()
  ((workers :type simple-vector :accessor workers :initarg :workers)
   (wait-cvar :initform (make-waitqueue :name "wait-cvar"))
   (wait-lock :initform (make-mutex :name "wait-lock"))
   (wait-count :initform (make-counter) :type counter)
   (notify-count :initform 0 :type (integer 0))
   (spin-count :type array-index :initarg :spin-count :initform *default-spin-count*)
   ;; cursor?
   (idx :initform 0 :type array-index :initarg :idx :accessor idx))
  (:documentation
   "A scheduler is responsible for finding and sequencing work to be executed by
WORKER threads."))

(defclass biased-scheduler (scheduler)
  ((low-priority-work
    :initform (make-spin-queue) 
    :type spin-queue 
    :accessor low-priority-work
    :initarg :low-priority-work))
  (:documentation "A biased scheduler with an additional spin-queue for 'low priority' work."))

(defun make-scheduler (workers spin-count &optional (class *scheduler-class*))
  (make-instance class :workers workers :spin-count spin-count))

(defmacro with-pop-success (sym queue &body body)
  (with-gensyms (presentp)
    `(multiple-value-bind (,sym ,presentp) (pop-spin-queue ,queue)
       (when ,presentp
	 ,@body))))

(defun push-to-random-worker (work sched)
  (declare (scheduler sched))
  (with-slots (workers) sched
    (push-spin-queue work (work (svref workers (mod-decf (idx sched) (length workers))))))
  (values))

(defmacro with-mutex-p ((mutex predicate &key (wait-p t) timeout) &body body)
  ;; eval PREDICATE twice!
  `(when ,predicate
     (with-mutex (,mutex :wait-p ,wait-p :timeout ,timeout)
       (when ,predicate
         ,@body))))

(defun maybe-wake-worker (sched)
  (declare (scheduler sched))
  (with-slots (wait-lock wait-cvar wait-count notify-count) sched
    (with-mutex-p (wait-lock (plusp (counter-value wait-count)))
      (incf notify-count)
      (condition-notify wait-cvar)))
  (values))

(defun schedule-work (sched work &optional priority)
  (declare (scheduler sched))
  (ccase priority
    (:low (with-slots (low-priority-work) sched (push-spin-queue work low-priority-work)))
    (:default (push-to-random-worker work sched))
    (t (push-to-random-worker work sched)))
  (maybe-wake-worker sched)
  (values))

(defmacro do-workers ((wvar workers hindex &optional from-hindex-p) &body body)
  (with-gensyms (wsvar ivar)
    `(let ((,wsvar ,workers))
       (declare (simple-vector ,wsvar))
       (do-indexes (,ivar (length (the simple-vector ,wsvar)) ,hindex ,from-hindex-p)
         (let ((,wvar (svref (the simple-vector ,wsvar) ,ivar)))
           (declare (worker ,wvar))
           ,@body)))))

(defun find-work (sched w)
  (declare (scheduler sched) (worker w))
  (labels ((try-pop (queue)
             (declare (type spin-queue queue))
             (with-pop-success work queue
               (return-from find-work work))
             (values))
           (try-pop-all ()
             (with-slots (workers) sched
               (do-workers (w workers (idx w) nil)
                 (try-pop (work w))))
             (values))
           (maybe-sleep ()
             (with-slots (wait-cvar wait-lock wait-count
                          notify-count low-priority-work) 
                 sched
               (inc-counter wait-count)
               (unwind-protect 
                    (with-mutex (wait-lock)
                      (try-pop (work w))
                      (try-pop low-priority-work)
                      (loop until (plusp notify-count)
                            do (condition-wait wait-cvar wait-lock)
                            finally (decf notify-count)))
                 (dec-counter wait-count)
                 (sb-thread::thread-yield)))
             (values)))
    (declare (dynamic-extent #'try-pop #'try-pop-all #'maybe-sleep))
    (with-slots (spin-count) sched
      (loop
        (try-pop (work w))
        (try-pop-all)
        (repeat spin-count
          (try-pop-all))
        (maybe-sleep)))))

(defun steal-work (scheduler)
  (declare (scheduler scheduler))
  (with-slots (workers idx low-priority-work) scheduler
    (let ((low-priority-work low-priority-work))
      (flet ((try-pop (work)
               (declare (spin-queue work low-priority-work))
               (with-pop-success w work
                 (when w ; don't steal nil, the end condition flag
                   (return-from steal-work w))
                 (push-spin-queue w low-priority-work))
               (values)))
        (declare (dynamic-extent #'try-pop))
        ;; Start with the worker that has the most recently submitted
        ;; work (approximately) and advance rightward.
        (do-workers (worker workers idx t)
          (try-pop (work worker)))
        (try-pop low-priority-work))))
  nil)

(defun steal-work* (pool worker)
  (when-let ((w (steal-work (scheduler pool))))
    (if worker
        (exec-with-worker w worker)
        (exec-without-worker w))
    t))

(defgeneric schedule (self &key &allow-other-keys))
(defgeneric (setf schedule) (new self &key &allow-other-keys))

;;; Supervisor
(defclass supervisor ()
  ((thread :initform (make-ephemeral-thread (symbol-name (gensym "supervisor"))) 
           :accessor supervisor-thread :initarg :thread)
   (scope :initarg :scope))
  (:documentation "Supervisors are threads which are responsible for a set of worker threads
within their DOMAIN and SCOPE."))

(defmethod initialize-instance :before ((self supervisor) &key name thread &allow-other-keys)
  (unless thread
    (when name
      (setf (supervisor-thread self) (make-ephemeral-thread (symbol-name (gensymify name)))))))

(defmethod initialize-instance :after ((self supervisor) &key &allow-other-keys)
  (push (supervisor-thread self) *super-threads*))

;;; Oracle
(defstruct (oracle (:constructor %make-oracle (id thread)))
  "Oracles provide a tagged view into some threaded scope of work."
  (id 0 :type (unsigned-byte 32) :read-only t)
  (thread *current-thread* :read-only t))

(defun oracle-of-id (id)
  (gethash id *oracle-table*))

(defun make-oracle (&optional (thread *current-thread*))
  (let ((id (thread-os-tid thread)))
    (if-let ((found (oracle-of-id id)))
      (values id found)
      (let ((orc (%make-oracle id thread)))
	(setf (gethash id *oracle-table*) (make-array 0 :adjustable t :fill-pointer 0))
	(values id orc)))))

;;; Thread Pool
(defclass thread-pool-context ()
  ((bind :initarg :bind :initform *default-special-bindings* :type list :accessor bind)
   (name :accessor name :initarg :name)))

(defclass thread-pool (thread-limiter thread-pool-context)
  ((kernel :type kernel :accessor kernel :initarg :kernel)
   (scheduler :initarg :scheduler :accessor scheduler)
   (workers :initarg :workers :accessor workers :type (simple-array worker))
   (lock :initarg :lock :initform (make-mutex :name "workers") :type mutex :accessor lock)
   (state :initform t :accessor state :type boolean :initarg :state))
  (:documentation "Thread pools are similar to LPARALLEL kernels - they encompass the scheduling
and execution of concurrent work using a pool of 'worker' threads."))

(definline register-thread-pool (name pool)
  (declare (thread-pool pool))
  (setf (gethash name *thread-pool-table*) pool))

(defun find-thread-pool (name) (gethash name *thread-pool-table*))

(defun list-all-thread-pools () (std/hash:hash-table-list *thread-pool-table*))

(defmethod call ((self thread-pool) args)
  (if (sb-int:singleton-p args)
      (funcall (kernel self) (car args) self)
      (apply (kernel self) args)))

(defmethod initialize-instance :after ((self thread-pool) &key name &allow-other-keys)
  (when name (register-thread-pool name self)))

(definline kill-workers (pool)
  "Call FINISH-THREADS on POOL's workers."
  (declare (thread-pool pool)
           (optimize (speed 3) (safety 0)))
  (dotimes (i (length (the (vector worker) (workers pool))))
    (kill-worker (svref (workers pool) i))))

(defun kill-thread-pool (pool)
  (assert pool)
  (let ((count (worker-count pool)))
    (with-slots (lock workers) pool
      ;; (with-mutex (lock)
      (kill-workers pool)
      (prog1 count
        (when *worker*
          (assert (eq (worker-thread *worker*) *current-thread*))
          ;; (when (eql category (running-category *worker*))
          (throw '#.+worker-suicide-tag+ nil))))))

(defun ensure-working-p (pool)
  (setf (state (the thread-pool pool)) t))

(defun update-limiter-count* (pool delta)
  (declare (thread-pool pool) (fixnum delta) 
           (optimize (speed 3) (safety 0)))
  (incf (the fixnum (limiter-count pool)) delta)
  (setf (state pool)
        (plusp (the fixnum (limiter-count pool))))
  (values))

;; REVIEW 2025-04-27: may need to add more to std/spin
(defun update-limiter-count (pool delta)
  (declare (thread-pool pool) (fixnum delta)
           (optimize (speed 3) (safety 0)))
  (with-mutex ((limiter-lock pool))
    (update-limiter-count* pool delta))
  (values))

;;;; Core
(defun exec-with-worker (work worker)
  (declare (worker worker))
  (funcall (kernel worker) work))

(defun exec-without-worker (work)
  (check-kernel)
  (call-with-work-handler (funcall *kernel* work)))

(defun replace-worker (pool worker)
  (with-slots (workers lock) pool
    (with-mutex (lock)
      (let ((i (position worker workers :test #'worker=)))
	(assert i)
	(assert (eql i (idx worker)))
	(warn "Replacing lost or dead worker")
        (unwind-protect-case ()
	                     (let ((new-worker 
                                     (make-instance 'worker
                                       :kernel (kernel worker)
                                       :idx i 
                                       :bind (worker-bind worker)
                                       :work (work worker))))
	                       (setf (svref workers i) new-worker)
	                       (send-worker-start new-worker)
	                       (receive-worker-status new-worker))
          (:abort (warn "Failed to replace worker - thread-pool is corrupt")))))))

(defun worker-loop (pool worker)
  (declare (thread-pool pool) (worker worker))
  (let ((sched (scheduler pool)))
    (declare (scheduler sched))
    (unwind-protect-case ()
	                 (loop (let ((work (find-work sched worker)))
		                 (if work
		                     (exec-with-worker work worker)
		                     (return))))
      (:abort (unless *lisp-exiting-p*
	        (replace-worker pool worker))))))

(defun call-with-worker-context (fn context pool worker)
  (receive-worker-start worker)
  (unwind-protect-case ()
                       (funcall context ; kernel
                                (lambda ()
                                  (let ((*worker* (find *current-thread* (workers pool)
                                                        :key #'worker-thread)))
                                    (assert *worker*)
                                    (send-worker-status worker :ok)
                                    (with-worker-restarts
                                      (%call-with-work-handler fn)))))
    ;; This error notification is seen when `worker-context' does not
    ;; call its worker-loop parameter, otherwise it's ignored.
    (:abort (send-worker-status worker :error))))

(defun enter-worker-loop (pool worker)
  (call-with-worker-context
   (lambda () (worker-loop pool worker))
   (kernel worker)
   pool
   worker))

(defun make-all-bindings (kernel bindings)
  (append bindings (list (cons '*kernel* kernel))))

(defun %make-worker (index class)
  (make-instance class :idx index :thread nil))

(defun make-worker-thread (pool worker &optional bind)
  (with-thread (:bindings (or bind (worker-bind worker)))
    (unwind-protect (enter-worker-loop pool worker)
      (notify-exit worker))))

(defun make-worker (pool index &optional work (class *worker-class*))
  (let* ((worker (%make-worker index class))
         (bind (make-all-bindings *worker-kernel* (bind pool)))
         (worker-thread (make-worker-thread pool worker bind)))
    (setf (worker-thread worker) worker-thread
          (worker-bind worker) bind)
    (when work (setf (work worker) work))
    worker))

(defmacro with-fill-workers-handler (workers &body body)
  `(unwind-protect-case () (progn ,@body)
     (:abort
      (map 'simple-vector
           (lambda (w)
             (when (typep w 'worker)
               (terminate-thread (worker-thread w))))
           ,workers))))

(defun %fill-workers (workers pool)
  (dotimes (i (length workers))
    (setf (aref workers i) (make-worker pool i (make-spin-queue)))))

(defun fill-workers (workers pool)
  (with-fill-workers-handler workers
    (%fill-workers workers pool)
    (map nil #'send-worker-start workers)
    (assert (every (lambda (x) (eql :ok x)) (map nil #'receive-worker-status workers)))))

;; (map nil #'receive-worker-start workers)))
;; (map nil #'receive-worker-start workers)))

(defun make-thread-pool (worker-count &key name
					   (bind `((*standard-output* . ,*standard-output*)
						   (*error-output* . ,*error-output*)))
					   (worker-kernel *worker-kernel*)
					   (spin-count *default-spin-count*)
                                           (alive t)
					   (kernel *pool-kernel*)
                                           enlist
                                           (class 'thread-pool)
                                           (worker-class *worker-class*)
                                           (scheduler-class *scheduler-class*))
  "Create a THREAD-POOL with WORKER-COUNT number of available worker threads.

NAME is an EQL-unique identifier associated with the thread-pool in
*THREAD-POOL-TABLE* (NIL is the default name).

BIND is an alist for establishing thread-local dynamic bindings inside worker
threads.

WORKER-KERNEL which begins the worker loop and returns when the worker exits.

KERNEL is a function which drives the THREAD-POOL.

CLASS is the designated class of the returned THREAD-POOL object.

SPIN-COUNT is the number of work-searching iterations done by the worker
before going to sleep.

When ENLIST is non-nil, the calling thread may be enlisted to steal work from
worker threads in certain situations."
  (check-type worker-count positive-fixnum)
  (check-type spin-count array-index)
  (let ((*worker-kernel* worker-kernel)
        (*pool-kernel* kernel)
        (*worker-class* worker-class))
    (let* ((workers (make-array worker-count))
           (count (if enlist (1+ worker-count) worker-count))
           (pool (make-instance class
                   :name name
		   :bind bind
	           :kernel *pool-kernel*
                   :state alive
                   :workers workers
	           :scheduler (make-scheduler workers spin-count scheduler-class)
	           :limiter-count (initial-limiter-count count)
	           :limiter-lock (make-mutex :name "limiter"))))
      (fill-workers workers pool)
      pool)))

(defun check-thread-pool ()
  "Check the current value of *THREAD-POOL*, ensuring it is bound to a
THREAD-POOL object. STORE-VALUE and MAKE-THREAD-POOL restarts are
provided. *THREAD-POOL* is returned."
  (or *thread-pool*
      (restart-case (error 'no-thread-pool-error)
        (make-thread-pool (name worker-count spin-count enlist alive)
          :report "Make a thread-pool now, prompting for arguments."
          :interactive (lambda () 
                         (list
                          (interact* "Name: ")
                          (interact* "Worker count: ")
                          (interact* "Spin count: ")
                          (y-or-n-p "Enlist calling thread?: ")
                          (y-or-n-p "Start?: ")))
          (setf *thread-pool* (make-thread-pool worker-count :name name :spin-count spin-count :enlist enlist :alive alive)))
        (make-default-thread-pool ()
          :report "Make a thread-pool named :DEFAULT, using (NUM-CPUS) as the worker count."
          (setf *thread-pool* (make-thread-pool (std/alien:num-cpus))))
        (store-value (value)
          :report "Assigne a value to *THREAD-POOL*."
          :interactive (lambda () (print "Value for *THREAD-POOL*: ") (read))
          (check-type value thread-pool)
          (setf *thread-pool* value)))))

(defmethod check ((self (eql :thread-pool)) &key) (check-thread-pool))

(defun worker-count (pool)
  "Return the worker count of POOL."
  (length (workers pool)))

(defun worker-count* ()
  "Return the worker count of *THREAD-POOL*."
  (worker-count *thread-pool*))

(defun worker-index* ()
  "If called from inside a worker return the worker's assigned index, ranging from 0 to (worker-count*)."
  (when-let ((worker *worker*))
    (idx worker)))

(defun workers* () (workers *thread-pool*))

(defun scheduler* () (scheduler *thread-pool*))

(defun start-workers (pool)
  "Start all workers in the given thread POOL."
  (loop for w across (workers pool)
        do (start-worker w)))

(defun start-workers* ()
  "Start all *thread-pool* workers."
  (start-workers *thread-pool*))

(defmethod designate-oracle ((self thread-pool) (guest thread))
  (let ((id (make-oracle guest)))
    (setf (gethash id *oracle-table*)
          (vector-push-extend (sb-ext:make-weak-pointer self) (gethash id *oracle-table*)))))

(defmethod designate-oracle ((self thread-pool) (guest (eql t)))
  (designate-oracle self *current-thread*))

(defmacro work-lambda (&body body)7
  "Generate a 'work-lambda' with BODY. *HANDLERS* will be bound for the duration
of the returned lambda."
  (with-gensyms (work handlers)
    `(flet ((,work () ,@body))
       (declare (optimize (speed 3) (safety 0)))
       (let ((,handlers *handlers*))
         (if ,handlers
             (lambda ()
               (let ((*handlers* ,handlers))
                 (,work)))
             #',work)))))

;; (defmacro super-lambda (&body body))

(defun make-channeled-work (channel fn args)
  (declare (channel channel) (function fn) (list args))
  (let ((queue (channel-queue channel))
        (ret))
    (work-lambda
      (unwind-protect-case 
          ()
          (progn
            (with-work-context (setf ret (apply fn args))) ; work handler handles everything
            ret)
        (:normal (push-queue ret queue))
        ;; unwind on kill
        (:abort (push-queue* (wrap-error 'worker-killed-error) queue))))))

;; make-work 
(defun submit-raw-work (work pool &optional (priority *work-priority*))
  (unless (state pool)
    (error "attempted to submit work to a dead thread-pool"))
  (schedule-work (scheduler pool) work priority))

(defun submit-work (ch fn &rest args)
  (check-type ch channel)
  (submit-raw-work
   (make-channeled-work ch
                        (ensure-function fn)
                        args)
   (channel-pool ch)))

(defun receive-result (channel)
  "Remove a result from CHANNEL. If nothing is available the call will block
until a result is received."
  (unwrap-result (pop-queue (channel-queue channel))))

(defun try-receive-result (channel &key timeout)
  "Attempt to remove a result from CHANNEL and return (values RESULT t).

By default if the channel is empty return (values nil nil)
immediately. TIMEOUT, if non-nil is the number of seconds to wait for a result
to appear on the queue."
  (multiple-value-bind (result presentp)
      (try-pop-queue (channel-queue channel) :timeout timeout)
    (if presentp
        (values (unwrap-result result) t)
        (values nil nil))))

(defmacro! do-fast-receives ((ret o!ch o!n) &body body)
  "Receive N results from channel CH, executing BODY each iteration with results
bound to RET."
  `(loop for i below ,g!n
         do (let ((,ret (receive-result ,g!ch)))
              ,@body)))

(defun steal-until-receive-result (channel worker fn)
  (loop
    (multiple-value-bind (result presentp) (try-receive-result channel)
      (when presentp
        (when fn
          (locally (declare (type function fn))
            (funcall fn result)))
        (return)))
    (steal-work* (channel-pool channel) worker)))

(defun receive-results (channel count fn)
  (let ((worker *worker*))
    (if worker
        (repeat count
          (steal-until-receive-result channel worker fn))
        (if fn
            (do-fast-receives (result channel count)
              (locally (declare (type function fn))
                (funcall fn result)))
            (do-fast-receives (result channel count)
              (declare (ignore result)))))))

(defmacro with-submit-counted (&body body)
  (with-gensyms (count channel)
    `(let ((,count 0)
           (,channel (make-channel)))
       (declare (fixnum ,count))
       (flet ((submit-counted (&rest args)
                (declare (dynamic-extent args))
                (apply #'submit-work ,channel args)
                (incf ,count))
              (receive-counted ()
                (receive-results ,channel ,count nil)))
         (declare (inline submit-counted receive-counted))
         ,@body))))

(defun shutdown-channel (channel pool)
  (let ((*work-priority* :low))
    (submit-work channel (lambda ())))
  (receive-result channel)
  (with-slots (workers scheduler state) pool
    (repeat (length workers)
      (schedule-work scheduler nil :low))
    (map nil #'wait-for-worker workers)
    (setf state nil)))

(definline stop-thread-pool (pool &key wait)
  (declare (thread-pool pool))
  (when (and pool (state pool))
    (let ((channel (make-channel :pool pool))
          (threads (map 'list #'worker-thread (workers pool))))
      (cond
        (wait
         (join-thread
          (make-thread 
           (lambda ()
             (shutdown-channel channel pool))
           :name (format nil "%shutdown-pool"))
          :timeout (and (numberp wait) wait))
         threads)
        (t
         (cons
          (with-thread (:name (format nil "%shutdown-pool"))
            (shutdown-channel channel pool))
          threads))))))

(definline start-thread-pool (pool)
  (declare (thread-pool pool))
  (setf (state pool) t)
  (ensure-working-p pool)
  (start-workers pool)
  pool)

(definline reset-thread-pool (pool)
  (declare (thread-pool pool))
  (stop-thread-pool pool :wait t)
  (start-thread-pool pool))
  
(defmethod shutdown ((pool thread-pool)) 
  (stop-thread-pool pool :wait t)
  (remhash (name pool) *thread-pool-table*))

(defmethod start ((pool thread-pool)) (start-thread-pool pool))

(defmethod stop ((pool thread-pool) &key) (stop-thread-pool pool))

(defmethod reset ((pool thread-pool) &key) (reset-thread-pool pool))

(defun end-thread-pool (&key wait)
  (when-let ((pool *thread-pool*))
    (stop-thread-pool pool :wait wait)
    (remhash (name pool) *thread-pool-table*)
    (setf *thread-pool* nil)))

(defun thread-pool-info (pool)
  (list :workers (worker-count pool)
        :alive (state pool)
        :spins (slot-value (scheduler pool) 'spin-count)
        :limit (limiter-count pool)))

(defmethod print-object ((pool thread-pool) stream)
  (print-unreadable-object (pool stream :type t :identity t)
    (format stream "~(~s ~^~)~{~s~^ ~}" (name pool) (thread-pool-info pool))))

(defun broadcast-work (function &rest args)
  "Wait for current and pending work to complete, if any, then
simultaneously execute the given work inside each worker. Wait until
this work is complete, then return the results in a vector.

Calling `broadcast-work' from inside a worker is an error."
  (when *worker*
    (error "Cannot call `broadcast-work' from inside a worker."))
  (let* ((function (ensure-function function))
	 (*thread-pool* (check-thread-pool))
         (worker-count (worker-count*))
         (channel (make-instance 'channel))
         (from-workers (make-semaphore :name "from-workers"))
         (to-workers (make-semaphore :name "to-workers")))
    (loop repeat worker-count 
          do (submit-work channel (lambda ()
                                    (signal-semaphore from-workers)
                                    (wait-on-semaphore to-workers)
                                    (apply function args))))
    (loop repeat worker-count
          do (wait-on-semaphore from-workers))
    (loop repeat worker-count 
          do (signal-semaphore to-workers))
    (map-into (make-array worker-count) (lambda () (receive-result channel)))))

(defun exit-workers ()
  (setf *lisp-exiting-p* t))

(defun exit-thread-pools ()
  (std/hash:maphash-values (lambda (x) (stop-thread-pool x :wait 2)) *thread-pool-table*)
  ;; (setf *thread-pool-table* (make-hash-table))
  )

(pushnew 'exit-workers sb-ext:*exit-hooks*)
(pushnew 'exit-workers sb-ext:*save-hooks*)
(pushnew 'exit-thread-pools sb-ext:*exit-hooks*)
(pushnew 'exit-thread-pools sb-ext:*save-hooks*)

;;; Utils
(defmacro with-timeout* ((seconds timeout-form) &body body)
  "Runs BODY as an implicit PROGN with timeout of SECONDS. If
timeout occurs before BODY has finished, BODY is unwound and
TIMEOUT-FORM is executed with its values returned instead.

Note that BODY is unwound asynchronously when a timeout occurs,
so unless all code executed during it -- including anything
down the call chain -- is asynch unwind safe, bad things will
happen. Use with care."
  (let ((exec (gensym)) (unwind (gensym)) (timer (gensym))
        (timeout (gensym)) (block (gensym)))
    `(block ,block
       (tagbody
          (flet ((,unwind ()
                   (go ,timeout))
                 (,exec ()
                   ,@body))
            (declare (dynamic-extent #',exec #',unwind))
            (let ((,timer (sb-ext:make-timer #',unwind)))
              (sb-sys:without-interrupts
                  (unwind-protect
                       (progn
                         (sb-ext:schedule-timer ,timer ,seconds)
                         (return-from ,block
                           (sb-sys:with-local-interrupts
                               (,exec))))
                    (sb-ext:unschedule-timer ,timer)))))
          ,timeout
          (return-from ,block ,timeout-form)))))

(defmacro with-lock-no-wait (lock predicate &body body)
  ;; predicate intentionally evaluated twice
  (with-gensyms (lock-var)
    `(when ,predicate
       (let ((,lock-var ,lock))
         (when (grab-mutex ,lock-var :waitp nil)
           (unwind-protect
                (when ,predicate
                  ,@body)
             (release-mutex ,lock-var)))))))

(defmacro with-lock-wait (lock predicate &body body)
  ;; predicate intentionally evaluated twice
  `(when ,predicate
     (with-mutex (,lock)
       (when ,predicate
         ,@body))))

(defun indexing-wrapper (array index function args)
  (setf (aref array index) (apply function args)))

(defmacro! with-submit-indexed (o!count o!array &body body)
  (with-gensyms (channel)
    `(let ((,channel (make-instance 'channel)))
       (flet ((submit-indexed (index function &rest args)
		(submit-work
		 ,channel #'indexing-wrapper ,g!array index function args))
	      (receive-indexed ()
		(receive-results ,channel ,g!count nil)
		,g!array))
	 (declare (inline submit-indexed receive-indexed))
	 ,@body))))

(defmacro submit-with-cancel (&body body)
  (with-gensyms (canceledp channel count)
    `(let ((,canceledp nil)
           (,count 0)
           (,channel (make-channel)))
       (flet ((submit-cancelable (fn &rest args)
                (submit-work ,channel
                             (lambda ()
                               (if ,canceledp
                                   'work-canceled
                                   (apply fn args))))
                (incf ,count)))
         (macrolet ((receive-cancelables (&optional fn)
                      `(receive-results ,',channel ,',count ,fn)))
           (unwind-protect (progn ,@body)
             (setf ,canceledp t)))))))

(defun call-with-temp-pool (fn &rest args)
  ;; ensure that we end the same pool we create
  (let ((pool (apply #'make-thread-pool args)))
    (prog1 (let ((*thread-pool* pool))
             (funcall fn))
      (stop-thread-pool pool))))

(defmacro with-temp-pool ((&rest make-pool-args) &body body)
  "Create a temporary pool for the duration of `body', ensuring that
`end-thread-pool' is eventually called. `make-thread-pool' is given the
arguments `make-pool-args'.

**NOTE**: Use this only if you understand its implications. Since
`*thread-pool*' is unaffected outside `body', the REPL will be useless with
respect to the temporary pool. For instance calling `kill'
from the REPL will not affect work that is running in the temporary
pool.

Multiple uses of `with-temp-pool' within the same application are
prone to defeat the purpose and benefits of having a thread pool. This
is an especial risk if `with-temp-pool' appears inside a library,
which is likely to be a suboptimal situation.

While using `with-temp-pool' is generally a bad idea, there are a
few valid uses, such as for testing, where the code is non-critical or
where convenience trumps other concerns."
  `(call-with-temp-pool (lambda () ,@body) ,@make-pool-args))

(defmacro with-thread-pool ((name &key kernel shutdown) &body body)
  "Lookup NAME in *THREAD-POOL-TABLE* and bind it to *THREAD-POOL* for the
duration of BODY.

When KERNEL is non-nil it will be installed in the kernel slot of the target
thread-pool.

When SHUTDOWN is T the thread-pool will be destroyed at the end of BODY, when
it is (eql :WAIT) the calling thread will wait for the thread-pool to finish
before returning."
  `(let ((*thread-pool* (find-thread-pool ,name)))
     ,@(when kernel `((setf (kernel *thread-pool*) ,kernel)))
     (unwind-protect (progn ,@body)
       ,@(when shutdown `((end-thread-pool ,@(when (eql shutdown :wait) '(:wait t))))))))

(defmacro with-channel (sym &body body)
  (let ((bind (if (consp sym) `(,(car sym) (make-channel :capacity ,(second sym)))
                  `(,sym (make-channel)))))
    `(let (,bind) ,@body)))

;;; Pipes
;; From Shinmera's VERBOSE
(defstruct sync-message
  (condition (make-waitqueue))
  (lock (make-mutex)))

(defmethod lock ((self sync-message)) (sync-message-lock self))

(defmethod msg ((vector vector) (msg sync-message))
  ;; ensure we're waiting on the condition..
  (with-mutex ((sync-message-lock msg))
    (condition-notify (sync-message-condition msg))))

(defmacro with-sync-message (s &body body)
  `(let ((,s (make-sync-message)))
     (with-mutex ((sync-message-lock ,s))
       ,@body
       (condition-wait* (sync-message-condition ,s) (sync-message-lock ,s)))))

(defclass thread-pipe (thread-pool) ()
  (:default-initargs :workers (make-pipe))
  (:documentation "A cross between a THREAD-POOL and a PIPE. The WORKERS slot
  is a pipe which gets passed messages and events directly."))

(defclass source-worker (worker source) ()
  (:documentation "A worker which is also the source of a PIPE."))

(defclass sink-worker (worker sink) ()
  (:documentation "A worker which is also a sink element of a PIPE."))

(defclass filter-worker (worker filter) ()
  (:documentation "A worker which acts as a filter element of a PIPE."))

(defclass worker-message (message) ()
  (:documentation "A message which may be passed between worker threads."))

(defclass worker-event (event) ())
;;; Message Box (MBOX)
(defclass message-box ()
  ())
;; (defstruct async-message ...)

;; shared message-box, driven by thread-pool
(defclass mbox (message-box) ())
;; owned message-box, driven by a worker
(defkernel mbox-worker (message-box worker) ())
