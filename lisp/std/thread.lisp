;;; threads.lisp --- Multi-thread utilities

;; Threading Macros

;;; Commentary:

;; mostly yoinked from sb-thread and friends

;;; Code:
(in-package :std)

(defun thread-support-p () (member :thread-support *features*))

(defun print-thread-info (&optional (stream *standard-output*))
  (let* ((curr-thread sb-thread:*current-thread*)
         (curr-thread-name (sb-thread:thread-name curr-thread))
         (all-threads (sb-thread:list-all-threads)))
	(format stream "Current thread: ~a~%~%" curr-thread)
	(format stream "Current thread name: ~a~%~%" curr-thread-name)
	(format stream "All threads:~% ~{~a~%~}~%" all-threads)))

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

(defmacro def-thread (name)
  `(progn
     (defstruct (,name
                 (:copier nil)
                 (:include thread (%name ,(string-downcase (symbol-name name))))
                 (:constructor ,(symbolicate 'make- name))
                 (:conc-name "THREAD-")))))

(defun make-threads (n fn &key (name "thread"))
  (loop for i from 1 to n
        collect (make-thread fn :name (format nil "~A-~D" name i))))

(defmacro with-threads ((idx n) &body body)
  `(make-threads ,n (lambda (,idx) (declare (ignorable ,idx)) ,@body)))

(defun finish-threads (&rest threads)
  (let ((threads (flatten threads)))
    (unwind-protect
         (mapc #'join-thread threads)
      (dolist (thread threads)
        (when (thread-alive-p thread)
          (destroy-thread thread))))))

(defun timed-join-thread (thread timeout)
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

;; from sb-thread
(defun dump-thread ()
  (let* ((primobj (sb-vm::primitive-object 'sb-vm::thread))
         (slots (sb-vm::primitive-object-slots primobj))
         (sap (current-thread-sap))
         (thread-obj-len (sb-vm::primitive-object-length primobj))
         (names (make-array thread-obj-len :initial-element "")))
    (loop for slot across slots
          do
          (setf (aref names (sb-vm::slot-offset slot)) (sb-vm::slot-name slot)))
    (flet ((safely-read (sap offset &aux (bits (sb-vm::sap-ref-word sap offset)))
             (cond ((eql bits sb-vm:no-tls-value-marker) :no-tls-value)
                   ((eql (logand bits sb-vm:widetag-mask) sb-vm:unbound-marker-widetag) :unbound)
                   (t (sb-vm::sap-ref-lispobj sap offset))))
           (show (sym val)
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

;;; Tasks
(defstruct task-queue
  (jobs (sb-concurrency:make-queue :name "jobs"))
  (workers (sb-concurrency:make-mailbox :name "workers"))
  (results (sb-concurrency:make-queue :name "results"))
  (completed-jobs 0 :type fixnum) ;;atomic
  (completed-tasks 0 :type fixnum))

(defparameter *task-queue* nil)

(defclass task ()
  ((object :initarg :object :accessor task-object)))

(defclass job ()
  ((stack :initform (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t)
          :initarg :stack
          :accessor :job-stack
          :type (vector task))))
