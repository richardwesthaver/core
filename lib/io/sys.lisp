;;; sys.lisp --- Linux System IO

;; Syscall condition handling, Timeouts, and IO Timers

;;; Commentary:

;; Attempts have been made to use SBCL internals as much as possible over
;; IOLib-style re-implementations, however it turns out in most cases the
;; duplicates are needed in order to extend functionality without interfering
;; with SBCL internals.

;;; Code:
(in-package :io/sys)

;;; Conditions
(define-condition sys-condition () ()
  (:documentation "Base class for all IO/SYS conditions."))
(define-condition sys-error (error sys-condition) ())
(define-condition io-syscall-error (sys-error std-error syscall-error) ())
(define-condition poll-error (io-syscall-error)
  ((type :initarg :type :reader error-type))
  (:report (lambda (c s)
             (format s "Poll error(event ~S)" (error-type c))
             (when (error-message c) (format s ": ~A" (error-message c)))))
  (:documentation
   "Signaled when an error occurs while polling for I/O readiness
of a file descriptor."))
(define-condition poll-timeout (poll-error) ()
  (:report (lambda (c s)
             (format s "Poll timeout(event ~S)" (error-type c))
             (when (error-message c) (format s ": ~A" (error-message c)))))
  (:documentation
   "Signaled when a timeout occurs while polling for I/O readiness
of a file descriptor."))

(defun syscall-error-p (thing)
  (typep thing 'syscall-error))

(defun io-syscall-error-p (thing)
  (typep thing 'io-syscall-error))

(defun io-syscall-error (control-string &rest args)
  (error 'io-syscall-error :message (format nil "~?" control-string args)))

(defmacro repeat-upon-condition ((&rest conditions) &body body)
  (with-gensyms (block-name)
    `(loop :named ,block-name :do
       (ignore-some-conditions ,conditions
         (return-from ,block-name (progn ,@body))))))

(definline handle-eintr (syscall)
  (let (v e)
    (loop (multiple-value-setq (v e) syscall)
          (unless (eql sb-posix:eintr e)
            (return (values v e))))))

(defmacro with-eintr-restart (syscall &body body)
    `(progn (handle-eintr ,syscall) ,@body))

(defmacro repeat-upon-eintr (&body body)
  `(repeat-upon-condition (eintr) ,@body))

(defmacro repeat-decreasing-timeout
    ((timeout-var timeout &optional (block-name nil blockp)) &body body)
  (unless (find timeout-var (flatten body))
    (warn "You probably want to use ~S inside the body ~A" timeout-var body))
  (unless blockp (setf block-name (gensym "BLOCK")))
  (with-gensyms (deadline temp-timeout)
    `(let* ((,timeout-var ,timeout)
            (,deadline (when ,timeout-var
                         (+ ,timeout-var (get-internal-real-time)))))
       (loop :named ,block-name :do
         ,@body
           (when ,deadline
             (let ((,temp-timeout (- ,deadline (get-internal-real-time))))
               (setf ,timeout-var
                     (if (plusp ,temp-timeout)
                         ,temp-timeout
                         0))))))))

(defmacro repeat-upon-condition-decreasing-timeout
    (((&rest conditions) timeout-var timeout &optional (block-name nil blockp)) &body body)
  (unless blockp (setf block-name (gensym "BLOCK")))
  `(repeat-decreasing-timeout (,timeout-var ,timeout ,block-name)
     (ignore-some-conditions ,conditions
       (return-from ,block-name (progn ,@body)))))

(defmacro repeat-syscall-decreasing-timeout (((&rest ints) timeout-var timeout &optional (block-name nil blockp))
                                             &body body)
  (unless blockp (setf block-name (gensym "BLOCK")))
  (with-gensyms (ret)
    `(repeat-decreasing-timeout (,timeout-var ,timeout ,block-name)
       (let ((,ret (progn ,@body) ))
         (case (print (sb-alien:get-errno))
           ,@(mapcar (lambda (x) `(,x)) ints)
           (t (return-from ,block-name ,ret)))))))

;;;; Syscall Errors
(defvar *syscall-error-table* (make-hash-table))

;; TODO 2026-03-13: 
(macrolet
    ((define-syscall-errors (keywords)
       `(progn
          ,@(loop for kw in keywords collect
               (let ((cond-name (intern (symbol-name kw)))
                     (code (err kw)))
                 `(progn
                    (define-condition ,cond-name (io-syscall-error) ()
                      (:default-initargs :errno ,code :name ,kw :message ,(sb-int:strerror code)))
                    (setf (gethash ,code *syscall-error-table*) ',cond-name)))))))
  (define-syscall-errors
      #.(alien-enum-keys 'err)))

;;; Timeouts
(deftype timeout ()
  'double-float)

(deftype timeout-designator ()
  '(or non-negative-real (member t nil)))

(deftype positive-timeout-designator ()
  '(or non-negative-real (eql t)))

;;; Break a real timeout into seconds and microseconds.
(defun decode-timeout (timeout)
  (assert (or (not timeout)
              (and (typep timeout 'real)
                   (not (minusp timeout))))
          (timeout)
          "The timeout must be a non-negative real or NIL: ~S" timeout)
  (typecase timeout
    (null nil)
    (integer (values timeout 0))
    (real
     (multiple-value-bind (q r) (truncate (coerce timeout 'timeout))
       (declare (type unsigned-byte q)
                (type timeout r))
       (values q (the (values unsigned-byte t) (truncate (* r 1d6))))))))

(defun normalize-timeout (timeout)
  (assert (and (typep timeout 'real)
               (not (minusp timeout)))
          (timeout)
          "The timeout must be non-negative: ~A" timeout)
  (coerce timeout 'timeout))

(defun clamp-timeout (timeout &optional (min 0) (max most-positive-fixnum))
  (clamp (or timeout most-positive-fixnum)
         (if min (max min 0) 0) (or max most-positive-fixnum)))

(defun timeval-from-timeout (timeout tv)
  (with-alien-slots (std/alien::tv-sec std/alien::tv-usec) tv
    (multiple-value-bind (%sec %usec)
        (decode-timeout timeout)
      (setf std/alien::tv-sec  %sec
            std/alien::tv-usec %usec))))

(defun timespec-from-timeout (timeout ts)
  (with-alien-slots (std/alien::tv-sec std/alien::tv-nsec) ts
    (multiple-value-bind (%sec %usec)
        (decode-timeout timeout)
      (setf std/alien::tv-sec  %sec
            std/alien::tv-nsec (* 1000 %usec)))))

(defun timeout-ms (timeout)
  (if timeout
      (multiple-value-bind (sec usec)
          (decode-timeout timeout)
        (+ (* sec 1000)
           (truncate usec 1000)))
      -1))

;;; Timers
(defstruct (io-timer
             (:conc-name %io-timer-)
             (:constructor %make-io-timer (name function expire-time
                                           relative-time oneshot)))
  name
  ;; to call when the timer expires
  function
  ;; absolute expiry time
  expire-time
  ;; relative expiry time
  relative-time
  ;; when NIL, the timer is automatically rescheduled
  ;; when triggered
  oneshot)

(defmethod print-object ((object io-timer) stream)
  (print-unreadable-object (object stream)
    (format stream "IO-TIMER ~S, Timeout: [ ~A , ~A ], ~:[persistent~;oneshot~]"
            (%io-timer-name object)
            (%io-timer-relative-time object)
            (%io-timer-expire-time object)
            (%io-timer-oneshot object))))

(defun make-io-timer (function delay &key name oneshot)
  (flet ((abs-timeout (timeout)
           (+ (get-internal-real-time)
              (normalize-timeout timeout))))
    (let ((name (or name "(unnamed)")))
      (%make-io-timer name function (abs-timeout delay) delay oneshot))))

(defun io-timer-name (timer)
  (%io-timer-name timer))

(defun io-timer-expired-p (timer now &optional (delta 0.0d0))
  (assert (%io-timer-expire-time timer) ((%io-timer-expire-time timer))
          "Timer ~A must have an expiry time set." timer)
  (let ((compare-time (+ now delta)))
    (> compare-time (%io-timer-expire-time timer))))

(defun reset-io-timer (timer)
  (setf (%io-timer-expire-time timer) 0))

(defun peek-schedule (schedule)
  (pqueue-maximum schedule))

(defun time-to-next-timer (schedule)
  (when-let ((timer (peek-schedule schedule)))
    (%io-timer-expire-time timer)))

(defun dispatch-timer (timer)
  (funcall (%io-timer-function timer)))

(defun timer-reschedulable-p (timer)
  (symbol-macrolet ((relative-time (%io-timer-relative-time timer))
                    (oneshot (%io-timer-oneshot timer)))
    (and relative-time (not oneshot))))

(defun reschedule-timer (schedule timer)
  (incf (%io-timer-expire-time timer) (%io-timer-relative-time timer))
  (pqueue-insert schedule timer))

(defun expire-pending-timers (schedule now)
  (let ((expired-p nil)
        (timers-to-reschedule ()))
    (flet ((handle-expired-timer (timer)
             (when (timer-reschedulable-p timer)
               (push timer timers-to-reschedule))
             (dispatch-timer timer))
           (%return ()
             (dolist (timer timers-to-reschedule)
               (reschedule-timer schedule timer))
             (return-from expire-pending-timers expired-p)))
      (loop
         (let ((next-timer (peek-schedule schedule)))
           (unless next-timer (%return))
           (cond ((io-timer-expired-p next-timer now)
                  (setf expired-p t)
                  (handle-expired-timer (pqueue-extract-maximum schedule)))
                 (t
                  (%return))))))))

(defun schedule-io-timer (schedule timer)
  (pqueue-insert schedule timer)
  (values timer))

(defun unschedule-io-timer (schedule timer)
  (pqueue-remove schedule timer)
  (values timer))

(defun reschedule-timer-relative-to-now (timer now)
  (setf (%io-timer-expire-time timer)
        (+ now (%io-timer-relative-time timer))))
