;;; threads.lisp --- Multi-thread utilities

;; Threading Macros

;;; Commentary:

;; mostly yoinked from sb-thread and friends

;;; Code:
(in-package :std)

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
  (mapc (lambda (thread) (sb-thread:join-thread thread :default nil)) threads)
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

;; unix-getrusage  
;; 0,-1,-2
;; (multiple-value-list (sb-unix:unix-getrusage 0))
;; (setf sb-unix::*on-dangerous-wait* :error)

(defclass oracle ()
  ((thread :initarg :thread :accessor oracle-thread)))

(defgeneric make-oracle (thread)
  (:method ((thread thread))
    (make-instance 'oracle :thread thread)))

(defgeneric designate-oracle (host guest))

(defgeneric push-job (job pool))
(defgeneric push-task (task pool))
(defgeneric push-result (task pool))
(defgeneric push-worker (thread pool))
(defgeneric push-stage (stage pool))
(defgeneric start-task-pool (pool))
(defgeneric pause-task-pool (pool))
(defgeneric stop-task-pool (pool))

(defstruct task-pool
  (oracle nil :type (or null oracle))
  (jobs (sb-concurrency:make-queue :name "jobs"))
  (stages #() :type (vector stage))
  (workers #() :type (vector thread))
  (results (sb-concurrency:make-queue :name "results")))

(defmethod designate-oracle ((self task-pool) (guest oracle))
  (setf (task-pool-oracle self) guest)
  self)

(defmethod designate-oracle ((self task-pool) (guest thread))
  (designate-oracle self (make-oracle guest)))

(defmethod oracle-thread ((self task-pool))
  (oracle-thread (task-pool-oracle self)))

(defmethod push-worker ((worker thread) (pool task-pool))
  (vector-push worker (task-pool-workers pool)))

(defclass task ()
  ((object :initarg :object :accessor task-object)))

(defmethod push-result ((task task) (pool task-pool))
  (sb-concurrency:enqueue task (task-pool-results pool)))

(defstruct job
  (tasks (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
   :type (vector task)))

(defmethod push-task ((task task) (pool task-pool))
  (make-job :tasks (vector task)))

(defmethod push-job ((job job) (pool task-pool))
  (sb-concurrency:enqueue job (task-pool-jobs pool)))

(defclass stage ()
  ((jobs  :initform (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
          :initarg :jobs
          :accessor :jobs
          :type (vector job))))

(defmethod push-stage ((stage stage) (pool task-pool))
  (vector-push-extend stage (task-pool-stages pool)))
