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
(defvar *default-spin-count* 2000
  "Default value of the 'spin-count' argument to MAKE-THREAD-POOL.")

(defvar *debug-threads-p* t
  "When non-nil the debugger is invoked when an error goes unhandled in a
threaded context.")

(defvar *lisp-exiting-p* nil
  "True if the Lisp process is exiting - used for skipping auto-replacement of
killed workers during shutdown.")

(declaim (pool-kernel-function %pool))
(definline %pool (state)
  "Default pool-kernel-function, assumes *THREAD-POOL* is bound."
  (declare (optimize (speed 3) (safety 0)))
  (ecase state
    (:start (start-workers*))
    (:stop (stop-thread-pool *thread-pool*))
    (:reset (reset-thread-pool *thread-pool*))
    (:shutdown (end-thread-pool))
    (:kill (kill *thread-pool*)))
  (values))

(defvar *pool-kernel* (make-kernel #'%pool)
  "A function which drives THREAD-POOLs.")

(declaim (worker-kernel-function %work))
(definline %work (&optional work)
  "Default worker-kernel-function."
  (declare (optimize (speed 3) (safety 0)))
  (let ((work (or work (when-let ((w (work *worker*))) (pop-spin-queue w)))))
    (typecase work
      (function (funcall work))
      (null)
      (cons (apply (the function (car work)) (cdr work)))
      (t work))))

(defvar *worker-kernel* (make-kernel #'%work)
  "A kernel which drives WORKERs.")

;;; Globals
(sb-ext:defglobal *worker-threads* nil
    "list of worker threads.")
(sb-ext:defglobal *super-threads* nil
    "List of threads with supervisor privileges.")
(sb-ext:defglobal *oracle-table* (make-hash-table)
    "Hashtable containining (ID . ORACLE-SCOPE).")
(sb-ext:defglobal *thread-pool-table* (make-hash-table :weakness :value)
    "Hashtable containing (NAME . THREAD-POOL).")

;;; Conditions
(defvar *error-workers* nil
  "Track debugger popups in order to kill them.")

(defvar *error-workers-lock* (make-mutex :name "error workers")
  "Lock for *ERROR-WORKERS*.")

(defun invoke-transfer-error (error)
  "Equivalent to (invoke-restart 'transfer-error error)."
  (invoke-restart 'transfer-error error))

(defun transfer-error-report (stream)
  (format stream "Transfer this error to a dependent thread, if one exists."))

(defun condition-handler (condition)
  "Mimic the CL handling mechanism, calling handlers until one assumes
control (or not)."
  (loop for ((condition-type . handler) . rest) on *handlers*
	do (when (typep condition condition-type)
	     (let ((*handlers* rest))
	       (handler-bind ((condition #'condition-handler))
		 (funcall handler condition)))))
  (when (typep condition 'error)
    (invoke-transfer-error condition)))

(defconstant +work-tag+ 'my-work)

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
  `(catch +work-tag+ ,@body))

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

;; from sb-thread
(defun dump-thread ()
  "Dump the contents of THREAD."
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
    (*readtable*                 (copy-readtable nil))))

(defun compute-special-bindings (bindings)
  (remove-duplicates (append bindings +standard-io-bindings+)
		     :from-end t :key #'car))

(defvar *%current-thread*)

(defun establish-dynamic-env (thread function special-bindings trap-conditions)
  "Return a closure that binds the symbols in SPECIAL-BINDINGS and calls
FUNCTION."
  (let* ((bindings (compute-special-bindings special-bindings))
	 (specials (mapcar #'car bindings))
	 (values (mapcar (lambda (f) (eval (cdr f))) bindings)))
    (std/macs:named-lambda %establish-dynamic-env-wrapper ()
      (progv specials values
	(with-slots (%lock %return-values %exit-condition)
	    thread
	  (flet ((record-condition (c)
		   (with-mutex (%lock)
		     (setf %exit-condition c)))
		 (run-function ()
		   (let ((*%current-thread* nil))
		     ;; Wait until the thread creator has finished creating
		     ;; the wrapper.
		     (with-mutex (%lock)
		       (setf *%current-thread* (%get-thread-wrapper *%current-thread*)))
		     (let ((retval
			     (multiple-value-list (funcall function))))
		       (with-mutex (%lock)
			 (setf %return-values retval))
		       retval))))
	    (if trap-conditions
		(handler-case
		    (values-list (run-function))
		  (condition (c)
		    (record-condition c)))
		(handler-bind
		    ((condition #'record-condition))
		  (values-list (run-function))))))))))

;;; Channel
(defstruct (channel (:constructor %make-channel))
  (queue (make-queue) :type queue)
  (pool *thread-pool*))

(defaccessor queue ((self channel)) (channel-queue self))

(definline make-channel (&key (pool *thread-pool*) capacity)
  (%make-channel :queue (make-queue :capacity capacity) :pool pool))

;;; Limiter
(defclass thread-limiter ()
  ((accept-work-p :accessor accept-work-p :type boolean :initarg :accept-work-p)
   (limiter-lock :accessor limiter-lock :initarg :limiter-lock)
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
  ((thread :initform (make-ephemeral-thread (symbol-name (gensym "worker")))
	   :accessor worker-thread
	   :initarg :thread)
   (kernel :initform *worker-kernel* :accessor kernel)
   (work :accessor work :type spin-queue :initarg :work)
   (index :reader worker-index :type array-index :initarg :index :accessor index)
   (bind :type list :accessor worker-bind :initarg :bind :initform *default-special-bindings* :accessor bind)))

(defmethod initialize-instance :after ((self worker) &key &allow-other-keys)
  (push (worker-thread self) *worker-threads*))

(defun make-worker* (&key thread kernel bind index)
  (apply #'make-instance *worker-class*
	 `(,@(when thread `(:thread ,thread))
           :index ,(or index (random 1024))
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

(defun run-worker (worker &key bind wait)
  (when bind
    (setf (worker-bind worker) bind))
  (start-worker worker)
  (if wait (join-worker worker)
      worker))

(defmethod run-object ((self worker) &key)
  (run-worker self))

(defun run-with-worker (worker object &key wait)
  (run-worker worker :bind object :wait wait))

(definline thread= (a b) (and (sb-thread:thread-alive-p a) (sb-thread:thread-alive-p b) 
                              (= (thread-os-tid a) (thread-os-tid b))))

(definline worker= (a b) 
  (and a b
       (or
        (= (index a) (index b))
        (thread= (worker-thread a) (worker-thread b)))))

(defun kill-worker (worker) 
  (declare (worker worker))
  (let ((th (worker-thread worker)))
    (unless (null th)
      (remove th *worker-threads* :test 'thread=)
      (kill-thread th))))

(defun join-worker (worker)
  (declare (worker worker))
  (let ((th (worker-thread worker)))
    (remove th *worker-threads* :test 'thread=)
    (join-thread th)))

;; from pool
(defun send-worker-start (worker)
  (assert (sb-concurrency:open-gate (slot-value worker '%rx)) nil "Failed to start worker ~A" worker))

(defun receive-worker-status (worker)
  (ecase (pop-queue (slot-value worker '%tx))
    (ok)
    (error (error 'kernel-init-error))))

;; from worker
(defun receive-worker-start (worker)
  ;; (print-top-level (format nil "worker ~A starting...~%" (worker-index worker)))
  (let ((gate (slot-value worker '%rx)))
    (sb-concurrency:wait-on-gate gate)
    (assert (sb-concurrency:close-gate gate) nil "Worker hijacked? ~A" worker)))

(defun send-worker-status (worker status)
  (check-type status (member ok error))
  ;; (print-top-level (format nil "worker ~A status: ~A~%" (worker-index worker) status))
  (push-queue status (slot-value worker '%tx)))

(defun notify-exit (worker)
  (sb-concurrency:close-gate (slot-value worker '%rx)))

(defun wait-for-worker (worker &optional timeout)
  (assert (sb-concurrency:wait-on-gate (slot-value worker '%rx) :timeout timeout)))

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
   (index :initform 0 :type array-index :initarg :index :accessor scheduler-index))
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

(defun make-scheduler (workers spin-count)
  (make-instance *scheduler-class* :workers workers :spin-count spin-count))

(defmacro with-pop-success (sym queue &body body)
  (with-gensyms (presentp)
    `(multiple-value-bind (,sym ,presentp) (pop-spin-queue ,queue)
       (when ,presentp
	 ,@body))))

(defun push-to-random-worker (work sched)
  (declare (scheduler sched))
  (with-slots (workers) sched
    (push-spin-queue work (work (svref workers (mod-decf (scheduler-index sched) (length workers))))))
  (values))

(defmacro with-mutex-p ((mutex predicate &key (wait-p t) timeout) &body body)
  ;; intentially eval PREDICATE twice
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
               (do-workers (w workers (worker-index w) nil)
                 (try-pop (work w))))
             (values))
           (maybe-sleep ()
             (with-slots (wait-cvar wait-lock wait-count
                          notify-count low-priority-work) sched
               (inc-counter wait-count)
               (unwind-protect 
                    (with-mutex (wait-lock)
                      (try-pop (work w))
                      (try-pop low-priority-work)
                      (loop until (plusp notify-count)
                            do (condition-wait wait-cvar wait-lock)
                            finally (decf notify-count)))
                 (dec-counter wait-count)))
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
  (with-slots (workers index low-priority-work) scheduler
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
        (do-workers (worker workers index t)
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
  ((thread :initform (make-ephemeral-thread (symbol-name (gensym "supervisor"))) :accessor supervisor-thread)
   (domain)
   (scope))
  (:documentation "Supervisors are threads which are responsible for a set of worker threads
within their DOMAIN and SCOPE."))

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
  ((kernel :initform *pool-kernel* :type kernel :accessor kernel :initarg :kernel)
   (scheduler :initarg :scheduler :accessor scheduler)
   (workers :initarg :workers :accessor workers :type (simple-array worker))
   (lock :initarg :lock :initform (make-mutex :name "workers") :type mutex :accessor lock)
   (alive :initform t :reader alive :type boolean :initarg :alive))
  (:documentation "Thread pools are similar to LPARALLEL kernels - they encompass the scheduling
and execution of concurrent work using a pool of 'worker' threads."))

(definline register-thread-pool (name pool)
  (declare (thread-pool pool))
  (setf (gethash name *thread-pool-table*) pool))

(defun find-thread-pool (name) (gethash name *thread-pool-table*))

(defmethod initialize-instance :after ((self thread-pool) &key name &allow-other-keys)
  (when name (register-thread-pool name self)))

(definline kill-workers (pool)
  "Call FINISH-THREADS on POOL's workers."
  (declare (thread-pool pool)
           (optimize (speed 3) (safety 0)))
  (dotimes (i (length (the (vector worker) (workers pool))))
    (kill-worker (svref (workers pool) i))))

(defun kill (pool)
  (assert pool)
  (let ((count (worker-count pool)))
    (with-slots (lock workers) pool
      (with-mutex (lock)
        (kill-workers pool))
      (prog1 count
        (when *worker*
          (assert (eq (worker-thread *worker*) *current-thread*))
          ;; (when (eql category (running-category *worker*))
          (throw '#.+worker-suicide-tag+ nil))))))

(defmacro ensure-working-p (pool)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (accept-work-p (the thread-pool ,pool))))

(defun update-limiter-count* (pool delta)
  (declare (thread-pool pool) (fixnum delta) 
           (optimize (speed 3) (safety 0)))
  (incf (the fixnum (limiter-count pool)) delta)
  (setf (accept-work-p pool)
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
	(assert (eql i (worker-index worker)))
	(warn "Replacing lost or dead worker")
        (unwind-protect-case ()
	                     (let ((new-worker 
                                     (make-instance 'worker
                                       :kernel (kernel worker)
                                       :index i 
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
  (unwind-protect
       (funcall context ; kernel
                (lambda ()
                  (let ((*worker* (find *current-thread* (workers pool)
                                        :key #'worker-thread)))
                    (assert *worker*)
                    (send-worker-status worker 'ok)
                    (with-worker-restarts
                      (%call-with-work-handler fn)))))
    ;; This error notification is seen when `worker-context' does not
    ;; call its worker-loop parameter, otherwise it's ignored.
    (send-worker-status worker 'error)))

(defun enter-worker-loop (pool worker)
  (call-with-worker-context
   (lambda () (worker-loop pool worker))
   (kernel worker)
   pool
   worker))

(defun make-all-bindings (kernel bindings)
  (append bindings (list (cons '*kernel* kernel))))

(defun %make-worker (index class)
  (make-instance class :index index :thread nil))

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
    (map nil #'receive-worker-status workers)))

;; (map nil #'receive-worker-start workers)))
;; (map nil #'receive-worker-start workers)))

(defun make-thread-pool (worker-count &key (name :default)
					   (bind `((*standard-output* . ,*standard-output*)
						   (*error-output* . ,*error-output*)))
					   (worker-kernel *worker-kernel*)
					   (spin-count *default-spin-count*)
                                           (alive t)
					   (kernel *pool-kernel*)
                                           enlist
                                           (class 'thread-pool))
  "Create a THREAD-POOL with WORKER-COUNT number of available worker threads.

NAME when non-nil is an EQL-unique identifier associated with the thread-pool
in *THREAD-POOL-TABLE*.

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
        (*pool-kernel* kernel))
    (let* ((workers (make-array worker-count))
           (count (if enlist (1+ worker-count) worker-count))
           (pool (make-instance class
                   :name name
		   :bind bind
	           :kernel *pool-kernel*
	           :accept-work-p alive
                   :alive alive
                   :workers workers
	           :scheduler (make-scheduler workers spin-count)
	           :limiter-count (initial-limiter-count count)
	           :limiter-lock (make-spin-lock))))
      (fill-workers workers pool)
      pool)))

(defun check-thread-pool ()
  "Check the current value of *THREAD-POOL*, ensuring it is bound to a
THREAD-POOL object. STORE-VALUE and MAKE-THREAD-POOL restarts are
provided. *THREAD-POOL* is returned."
  (or *thread-pool*
      (restart-case (error 'no-thread-pool-error)
        (make-thread-pool (worker-count)
          :report "Make a thread-pool now, prompting for number of workers."
          :interactive (lambda () (princ "Worker count: ") (list (read)))
          (setf *thread-pool* (make-thread-pool worker-count)))
        (store-value (value)
          :report "Assigne a value to *THREAD-POOL*."
          :interactive (lambda () (print "Value for *THREAD-POOL*: ") (read))
          (check-type value thread-pool)
          (setf *thread-pool* value)))))

(defun worker-count (pool)
  "Return the worker count of POOL."
  (length (workers pool)))

(defun worker-count* ()
  "Return the worker count of *THREAD-POOL*."
  (worker-count *thread-pool*))

(defun worker-index* ()
  "If called from inside a worker return the worker's assigned index, ranging from 0 to (worker-count*)."
  (when-let ((worker *worker*))
    (worker-index worker)))

(defun workers* () (workers *thread-pool*))

(defun scheduler* () (scheduler *thread-pool*))

(defun start-workers (pool)
  "Start all workers in the given task POOL."
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

(defmacro work-lambda (&body body)
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

;; TODO 2025-04-30: 
(defmacro pool-lambda (state &body body)
  "Generate a 'pool-lambda' with provided BODY. *THREAD-POOL* and *HANDLERS* are
bound for the duration of the returned lambda and STATE is the name of the
single required argument of the lambda. The lambda should run all code
assigned to the input state and then return two values."
  (with-gensyms (handlers pool)
    `(labels ((*kernel* (,state) ,@body (values)))
       (declare (optimize (speed 3) (safety 0))
                (pool-kernel-function *kernel*)
                (inline *kernel*))
       (let ((,handlers *handlers*)
             (,pool *thread-pool*))
	 (if ,handlers
	     (lambda (,state)
	       (let ((*handlers* ,handlers)
                     (*thread-pool* ,pool))
		 (*kernel* ,state)))
             (lambda (,state)
               (let ((*thread-pool* ,pool))
	         (*kernel* ,state))))))))

;; (defmacro super-lambda (&body body))

;; (defmacro channel-lambda (ch &body body))

(defun make-channeled-work (channel fn args)
  (declare (channel channel) (function fn) (list args))
  (let ((queue (channel-queue channel)))
    (work-lambda
      (unwind-protect-case () 
                           (push-queue (with-work-context (apply fn args)) queue) ; work handler handles everything
        ;; unwind on kill
        (:abort (push-queue (wrap-error 'worker-killed-error) queue))))))

;; make-work 
(defun submit-raw-work (work pool &optional (priority *work-priority*))
  (unless (alive pool)
    (error "attempted to submit work to a dead thread-pool"))
  (schedule-work (scheduler pool) work priority))

(defun submit-work (ch fn &rest args)
  (check-type ch channel)
  (submit-raw-work
   (make-channeled-work ch
                        (std/curry:ensure-function fn)
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
  (with-slots (scheduler workers alive) pool
    (loop for i below (length workers)
          do (schedule-work scheduler nil :low))
    (map nil #'wait-for-worker workers)
    (setf alive nil)))

(definline stop-thread-pool (pool &key wait)
  (declare (thread-pool pool))
  (when (alive pool)
    (let ((channel (let ((*thread-pool* pool)) (make-instance 'channel)))
          (threads (map 'list #'worker-thread (workers pool))))
      (cond (wait
             (shutdown-channel channel pool)
             threads)
            (t
             (cons (with-thread (:name (format nil "%shutdown-pool"))
                     (shutdown-channel channel pool))
                   threads))))))

(definline reset-thread-pool (pool)
  (declare (thread-pool pool))
  (stop-thread-pool pool)
  pool)
  
(defmethod shutdown ((pool thread-pool)) (funcall (kernel pool) :shutdown))

(defmethod start ((pool thread-pool)) (funcall (kernel pool) :start))

(defmethod stop ((pool thread-pool) &key) (funcall (kernel pool) :stop))

(defmethod reset ((pool thread-pool) &key) (funcall (kernel pool) :reset))

(defun end-thread-pool (&key wait)
  (when-let ((pool *thread-pool*))
    (let ((name (when (slot-boundp pool 'name) (name pool))))
      (when name (remhash name *thread-pool-table*))
      (setf *thread-pool* nil
            *worker-threads* (flatten *worker-threads*)))
    (stop-thread-pool pool :wait wait)))

(defun thread-pool-info (pool)
  (list :workers (worker-count pool)
        :alive (alive pool)
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

(defun %exit-threads ()
  (setf *lisp-exiting-p* t))

(pushnew '%exit-threads sb-ext:*exit-hooks*)

;;; Utils
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
    (unwind-protect
         (let ((*thread-pool* pool))
           (funcall fn))
      (shutdown pool))))

(defmacro with-temp-pool ((&rest make-pool-args) &body body)
  "Create a temporary pool for the duration of `body', ensuring that
`end-thread-pool' is eventually called. `make-thread-pool' is given the
arguments `make-pool-args'.

**NOTE**: Use this only if you understand its implications. Since
`*thread-pool*' is unaffected outside `body', the REPL will be useless with
respect to the temporary pool. For instance calling `kill'
from the REPL will not affect tasks that are running in the temporary
pool.

Multiple uses of `with-temp-pool' within the same application are
prone to defeat the purpose and benefits of having a thread pool. This
is an especial risk if `with-temp-pool' appears inside a library,
which is likely to be a suboptimal situation.

While using `with-temp-pool' is generally a bad idea, there are a
few valid uses, such as for testing, where the code is non-critical or
where convenience trumps other concerns."
  `(call-with-temp-pool (lambda () ,@body) ,@make-pool-args))

;;; Pipes
;; From Shinmera's VERBOSE
(defstruct sync-message
  (condition (make-waitqueue))
  (lock (make-mutex)))

(defmethod lock ((self sync-message)) (sync-message-lock self))

(defmethod msg ((vector vector) (msg sync-message))
  ;; ensure we're waiting on the condition..
  (with-mutex ((sync-message-lock msg)))
  (condition-notify (sync-message-condition msg)))

(defmacro with-sync-message (s &body body)
  `(let ((,s (make-sync-message)))
     (with-mutex ((sync-message-lock ,s))
       ,@body
       (condition-wait* (sync-message-condition ,s) (sync-message-lock ,s)))))
