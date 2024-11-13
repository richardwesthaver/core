;;; bench/trivial/richards.lisp -- operating system simulation code

;; Based on code from CL-BENCH

;;; Commentary:

;; Original header follows:

;; Time-stamp: <2016-05-10 13:09:23 jack>


;; ======================================================================
;; Newsgroups: comp.lang.smalltalk
;; Distribution: comp
;; Subject: Smalltalk vs. C(++) performance
;; 
;; As some have pointed out, it is difficult to compare the runtime
;; performance of Smalltalk programs with the performance of equivalent C
;; programs.  One reason for this is that for most non-trivial programs
;; there is no equivalent program written in the other language (because
;; it would be a non-trivial effort to write it).
;; 
;; The "best" benchmark I know of is the Richards benchmark, an operating
;; system simulation.  It is written in an object-oriented style, uses
;; polymorphism, and is reasonably non-trivial (700 lines).  It's
;; probably not the world's greatest benchmark, but better than 
;; micro-benchamrks, and it is available in Smalltalk, Self, T (an
;; object-oriented version of Scheme) and C++.
;; 
;; [Historical note: the Richards benchmark was originally written in
;; BCPL by Mark Richards.  Many thanks to L. Peter Deutsch for the
;; Smalltalk version.]
;; 
;; Disclaimer: Richards is *not* a typical application: it is relatively
;; small and contains no graphics or other user interaction.  Thus it may
;; not reflect the relative performance of Your Own Real-World (TM)
;; Application, but I think it tests the efficiency of the basic language
;; mechanisms fairly well.

;;; Code:
(defpackage :core/bench/richards
  (:nicknames :bench/richards)
  (:use :cl)
  (:export #:richards))

(in-package :core/bench/richards)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant devicea 5)
  (defconstant deviceb 6)
  (defconstant device-packet-kind 1)
  (defconstant handlera 3)
  (defconstant handlerb 4)
  (defconstant idler 1)
  (defconstant no-work nil)
  (defconstant no-task nil)
  (defconstant worker 2)
  (defconstant work-packet-kind 2))

(defvar task-list no-task)
(defvar current-task nil)
(defvar current-task-identity nil)
(defvar task-table (make-array 6 :initial-element no-task))
(declaim (simple-vector taskTable))
(defvar tracing nil)
(defvar layout 0)
(defvar queue-packet-count 0)
(defvar hold-count 0)
(declaim (fixnum layout queue-packet-count hold-count))

(declaim (inline make-task-control-block make-packet make-device-task-data-record
		 make-handler-task-data-record make-idle-task-data-record
		 make-worker-task-data-record wait))

(defstruct (task-control-block (:constructor make-task-control-bLock ()))
  packet-pending task-waiting task-holding link identity
  (priority 0 :type fixnum)
  input state handle)

(defstruct (packet (:constructor make-packet ()))
  link identity
  (kind 0 :type fixnum)
  (datum 0 :type fixnum) 
  (data '#() :type simple-vector))

(defstruct (device-task-data-record (:constructor make-device-task-data-record ()))
  pending)

(defstruct (handler-task-data-record (:constructor make-handler-task-data-record ()))
  work-in device-in)

(defstruct (idle-task-data-record (:constructor make-idle-task-data-record ()))
  (control 0 :type fixnum)
  (count 0 :type fixnum))

(defstruct worker-task-data-record
  (destination 0 :type fixnum)
  (count 0 :type fixnum))

(defun wait ()
  (setf (task-control-block-task-waiting current-task) t)
  current-task)

(defun device-task-data-record-run (self work)
  (let ((function-work work))
    (if (eq no-work function-work)
	(progn
	 (setq function-work (device-task-data-record-pending self))
	 (if (eq no-work function-work)
	     (wait)
	   (progn
	    (setf (device-task-data-record-pending self) no-work)
	    (queue-packet function-work))))
      (progn
       (setf (device-task-data-record-pending self) function-work)
       (if tracing (trace-it (packet-datum function-work)))
       (hold-self)))))

(defun handler-task-data-record-run (self work)
  (if (eq no-work work)
      nil
    (if (= work-packet-kind (packet-kind work))
	(work-in-add self work)
      (device-in-add self work)))
  (let ((work-packet (handler-task-data-record-work-in self)))
    (if (eq no-work work-packet)
	(wait)
      (let ((count (packet-datum work-packet)))
	(if (> count 4)
	    (progn
	     (setf (handler-task-data-record-work-in self)
		   (packet-link work-packet))
	     (queue-packet work-packet))
	  (let ((device-packet (handler-task-data-record-device-in self)))
	    (if (eq no-work device-packet)
		(wait)
	      (progn
	       (setf (handler-task-data-record-device-in self)
		     (packet-link device-packet))
	       (setf (packet-datum device-packet)
		     (svref (packet-data work-packet) (- count 1)))
	       (setf (packet-datum work-packet) (+ count 1))
	       (queue-packet device-packet)))))))))

(defun idle-task-data-record-run (self work)
  (declare (ignore work))
  (setf (idle-task-data-record-count self)
	(- (idle-task-data-record-count self) 1))
  (if (= 0 (idle-task-data-record-count self))
      (hold-self)
    (if (= 0 (logand (idle-task-data-record-control self) 1))
	(progn
	 (setf (idle-task-data-record-control self)
	       (floor (idle-task-data-record-control self) 2))
	 (release devicea))
      (progn
       (setf (idle-task-data-record-control self)
	     (logxor (floor (idle-task-data-record-control self) 2)
		     53256))
       (release deviceb)))))

(defun worker-task-data-record-run (self work)
  (if (eq no-work work)
      (wait)
    (progn
     (setf (worker-task-data-record-destination self)
	   (if (= handlera (worker-task-data-record-destination self))
	       handlerb
	     handlera))
     (setf (packet-identity work) (worker-task-data-record-destination self))
     (setf (packet-datum work) 1)
     (do ((i 0 (+ i 1)))
	 ((> i 3) nil)
       (declare (fixnum i))
	 (setf (worker-task-data-record-count self)
	       (+ (worker-task-data-record-count self) 1))
	 (if (> (worker-task-data-record-count self) 256)
	     (setf (worker-task-data-record-count self) 1))
	 (setf (svref (packet-data work) i)
	       (the fixnum
		    (+ (char-code #\A)
		       (- (worker-task-data-record-count self) 1)))))
     (queue-packet work))))

(defun append-head (packet queue-head)
  (setf (packet-link packet) no-work)
  (if (eq no-work queue-head)
      packet
    (let ((mouse queue-head))
      (let ((link (packet-link mouse)))
	(do ()
	    ((eq no-work link) nil)
	    (setq mouse link)
	    (setq link (packet-link mouse)))
	(setf (packet-link mouse) packet)
	queue-head))))

(defun initialize-globals ()
  (setq task-list no-task)
  (setq current-task nil)
  (setq current-task-identity nil)
  (setq task-table (make-array 6 :initial-element no-task))
  (setq tracing nil)
  (setq layout 0)
  (setq queue-packet-count 0)
  (setq hold-count 0))

(defun richards (&optional (iterations 1000000))
  (initialize-globals)
  (create-idler idler 0 no-work (running (make-task-control-block)))
  (let ((workq))
    (setq workq (create-packet no-work worker work-packet-kind))
    (setq workq (create-packet workq worker work-packet-kind))
    (create-worker worker 1000 workq (waiting-with-packet))

    (setq workq (create-packet no-work devicea device-packet-kind))
    (setq workq (create-packet workq devicea device-packet-kind))
    (setq workq (create-packet workq devicea device-packet-kind))
    (create-handler handlera 2000 workq (waiting-with-packet))
    
    (setq workq (create-packet no-work deviceb device-packet-kind))
    (setq workq (create-packet workq deviceb device-packet-kind))
    (setq workq (create-packet workq deviceb device-packet-kind))
    (create-handler handlerb 3000 workq (waiting-with-packet))
    
    (create-device devicea 4000 no-work (waiting))
    (create-device deviceb 5000 no-work (waiting)))
  (dotimes (i iterations) (schedule))
  (values))

(defun schedule ()
  (setq current-task task-list)
  (do ()
      ((eq no-task current-task) nil)
    (if (is-task-holding-or-waiting current-task)
	(setq current-task (task-control-block-link current-task))
	(progn
	 (setq current-task-identity (task-control-block-identity current-task))
	 (when tracing (trace-it current-task-identity))
	 (setq current-task (run-task current-task))))))

(defun find-task (identity)
  (declare (fixnum identity))
  (let ((tk (svref task-table (- identity 1))))
    (if (eq no-task tk) (error "find-task failed"))
    tk))

(defun hold-self ()
  (setq hold-count (+ hold-count 1))
  (setf (task-control-block-task-holding current-task) t)
  (task-control-block-link current-task))

(defun queue-packet (packet)
  (let ((tk (find-task (packet-identity packet))))
    (if (eq no-task tk)
	no-task
	(progn
	 (setq queue-packet-count (+ queue-packet-count 1))
	 (setf (packet-link packet) no-work)
	 (setf (packet-identity packet) current-task-identity)
	 (add-input tk packet current-task)))))

(defun release (identity)
  (let ((tk (find-task identity)))
    (if (eq no-task tk)
	no-task
	(progn
	 (setf (task-control-block-task-holding tk) nil)
	 (if (> (task-control-block-priority tk)
		(task-control-block-priority current-task))
	     tk
	     current-task)))))

(defun trace-it (id)
  (setq layout (- layout 1))
  (if (>= 0 layout)
      (progn
       (format t "~%")
       (setq layout 30)))
  (format t "~a " id))

(defun create-device (identity priority work state)
  (let ((data (create-device-task-data-record)))
    (create-task identity priority work state data)))

(defun create-handler (identity priority work state)
  (let ((data (create-handler-task-data-record)))
    (create-task identity priority work state data)))

(defun create-idler (identity priority work state)
  (let ((data (create-idle-task-data-record)))
    (create-task identity priority work state data)))

(defun create-worker (identity priority work state)
  (let ((data (create-worker-task-data-record)))
    (create-task identity priority work state data)))

(defun create-task (identity priority work state data)
  (let ((tk (create-task-control-block
	     task-list identity priority work state data)))
    (setq task-list tk)
    (setf (svref task-table (- identity 1)) tk)))

(defun running (tcb)
  (setf (task-control-block-packet-pending tcb) nil)
  (setf (task-control-block-task-waiting tcb) nil)
  (setf (task-control-block-task-holding tcb) nil)
  tcb)

(defun waiting ()
  (let ((tcb (make-task-control-block)))
    (setf (task-control-block-packet-pending tcb) nil)
    (setf (task-control-block-task-waiting tcb) t)
    (setf (task-control-block-task-holding tcb) nil)
    tcb))

(defun waiting-with-packet ()
  (let ((tcb (make-task-control-block)))
    (setf (task-control-block-packet-pending tcb) t)
    (setf (task-control-block-task-waiting tcb) t)
    (setf (task-control-block-task-holding tcb) nil)
    tcb))

(defun is-task-holding-or-waiting (tcb)
  (or (task-control-block-task-holding tcb)
      (and (not (task-control-block-packet-pending tcb))
	   (task-control-block-task-waiting tcb))))

(defun is-waiting-with-packet (tcb)
  (and (task-control-block-packet-pending tcb)
       (and (task-control-block-task-waiting tcb)
	    (not (task-control-block-task-holding tcb)))))

(defun packet-now-pending (tcb)
  (setf (task-control-block-packet-pending tcb) t)
  (setf (task-control-block-task-waiting tcb) nil)
  (setf (task-control-block-task-holding tcb) nil)
  tcb)

(defun create-task-control-block
  (link identity priority initial-work-queue initial-state private-data)
  (let ((r (make-task-control-block)))
    (setf (task-control-block-link r) link)
    (setf (task-control-block-identity r) identity)
    (setf (task-control-block-priority r) priority)
    (setf (task-control-block-input r) initial-work-queue)
    (setf (task-control-block-packet-pending r)
	 (task-control-block-packet-pending initial-state))
    (setf (task-control-block-task-waiting r)
	 (task-control-block-task-waiting initial-state))
    (setf (task-control-block-task-holding r)
	 (task-control-block-task-holding initial-state))
    (setf (task-control-block-handle r) private-data)
    (setf (task-control-block-state r) nil)
    r))

(defun add-input (tcb packet old-task)
  (if (eq no-work (task-control-block-input tcb))
      (progn
       (setf (task-control-block-input tcb) packet)
       (setf (task-control-block-packet-pending tcb) t)
       (if (> (task-control-block-priority tcb)
	      (task-control-block-priority old-task))
	   tcb
	   old-task))
      (progn
       (setf (task-control-block-input tcb)
	    (append-head packet (task-control-block-input tcb)))
       old-task)))

(defun run-task (tcb)
  (let ((message nil))
    (if (is-waiting-with-packet tcb)
	(progn
	  (setq message (task-control-block-input tcb))
	  (setf (task-control-block-input tcb) (packet-link message))
	  (if (eq no-work (task-control-block-input tcb))
	      (running tcb)
	    (packet-now-pending tcb)))
      (setq message no-work))
    (run (task-control-block-handle tcb) message)))

(defun run (self work)
  (typecase self
	    (device-task-data-record (device-task-data-record-run self work))
	    (handler-task-data-record (handler-task-data-record-run self work))
	    (idle-task-data-record (idle-task-data-record-run self work))
	    (worker-task-data-record (worker-task-data-record-run self work))))

(defun create-packet (link identity kind)
  (let ((p (make-packet)))
    (setf (packet-link p) link)
    (setf (packet-identity p) identity)
    (setf (packet-kind p) kind)
    (setf (packet-datum p) 1)
    (let ((v (make-array 4 :initial-element 0)))
      (setf (packet-data p) v))
    p))

(defun create-device-task-data-record ()
  (let ((tk (make-device-task-data-record)))
    (setf (device-task-data-record-pending tk) no-work)
    tk))

(defun create-handler-task-data-record ()
  (let ((tk (make-handler-task-data-record)))
    (setf (handler-task-data-record-work-in tk) no-work)
    (setf (handler-task-data-record-device-in tk) no-work)
    tk))

(defun device-in-add (tk packet)
  (setf (handler-task-data-record-device-in tk)
       (append-head packet (handler-task-data-record-device-in tk)))
  tk)

(defun work-in-add (tk packet)
  (setf (handler-task-data-record-work-in tk)
       (append-head packet (handler-task-data-record-work-in tk)))
  tk)

(defun create-idle-task-data-record ()
  (let ((tk (make-idle-task-data-record)))
    (setf (idle-task-data-record-control tk) 1)
    (setf (idle-task-data-record-count tk) 10000)
    tk))

(defun create-worker-task-data-record ()
  (let ((tk (make-worker-task-data-record)))
    (setf (worker-task-data-record-destination tk) handlerA)
    (setf (worker-task-data-record-count tk) 0)
    tk))
