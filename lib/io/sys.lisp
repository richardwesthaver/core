;;; sys.lisp --- Linux System IO

;; Syscall condition handling, Timeouts, and IO Timers

;;; Commentary:

;; Attempts have been made to use SBCL internals as much as possible over
;; IOLib-style re-implementations, however it turns out in most cases the
;; duplicates are needed in order to extend functionality without interfering
;; with SBCL internals.

;;; Code:
(in-package :io/sys)

;;; Variables
(defvar *syscall-error-table* (make-hash-table))

;;; Conditions
(define-condition sys-condition () ()
  (:documentation "Base class for all IO/SYS conditions."))
(define-condition io-syscall-error (sys-condition syscall-error std-error) 
  ((handlers :initarg :handlers :reader error-handlers))
  (:report (lambda (c s) 
             (format s "Syscall error ~S ~A" (syscall-name c) (syscall-errno c))
             (when (error-message c) (format s ": ~A" (error-message c))))))
(define-condition poll-error (io-syscall-error)
  ((fd :initarg :fd :reader error-fd)
   (type :initarg :type :reader error-type))
  (:report (lambda (c s)
             (format s "Poll error(event ~S, fd ~A)" (error-type c) (error-fd c))
             (when (error-message c) (format s ": ~A" (error-message c)))))
  (:documentation
   "Signaled when an error occurs while polling for I/O readiness
of a file descriptor."))
(define-condition poll-timeout (poll-error) ()
  (:report (lambda (c s)
             (format s "Poll timeout(event ~S, fd ~A)" (error-type c) (error-fd c))
             (when (error-message c) (format s ": ~A" (error-message c)))))
  (:documentation
   "Signaled when a timeout occurs while polling for I/O readiness
of a file descriptor."))

(defun syscall-never (errcode syscall)
  (declare (ignore errcode syscall))
  nil)

(definline get-syscall-error (code)
  (gethash code *syscall-error-table*))

(defun syscall-error-predicate (alien-type)
  (typecase alien-type
    (sb-alien::alien-c-string-type '(lambda (s) (not (stringp s))))
    (sb-alien::alien-pointer-type 'sb-alien:null-alien)
    (sb-alien::alien-integer-type
     (if (sb-alien::alien-integer-type-signed alien-type)
         'minusp
         'syscall-never))
    (sb-alien::alien-values-type
     (if (sb-alien::alien-void-type-p alien-type)
         'syscall-never
         ;; WARNING: assumes only 1 value
         (syscall-error-predicate (car (sb-alien::alien-values-type-values alien-type)))))
    (t (error "Could not choose an error-predicate function."))))

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
  "Call SYSCALL with interrupts handled (in second value)."
  (let (v e)
    (loop (multiple-value-setq (v e) syscall)
          (unless (eql sb-posix:eintr e)
            (return (values v e))))))

(defun get-monotonic-time ()
  "Get the monotonic NON-COARSE realtime. This is twice as slow as COARSE according to SBCL (test)."
  (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ seconds (/ nanoseconds 1d9))))

(defmacro with-eintr-restart (syscall &body body)
  `(handle-eintr (io-syscall ,syscall (progn ,@body))))

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
                         (+ ,timeout-var (get-monotonic-time)))))
       (loop :named ,block-name :do
                ,@body
                (when ,deadline
                  (let ((,temp-timeout (- ,deadline (get-monotonic-time))))
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

;;;; Syscall Errors

;; TODO 2026-03-13: 
(macrolet
    ((define-syscall-errors (keywords)
       `(progn
          ,@(loop for kw in keywords collect
                     (let ((cond-name (intern (symbol-name kw)))
                           (code (err kw)))
                       `(progn
                          (deferror ,cond-name (io-syscall-error) ()
                            (:default-initargs :errno ,code :name ,kw :message ,(strerror code))
                            (:auto t))
                          (setf (gethash ,code *syscall-error-table*) ',cond-name)))))))
  (define-syscall-errors
      #.(alien-enum-keys 'err)))

;;; Generic Functions
(defverb fd (self) (:accessor t))

;;; Syscall wrappers
;; TODO 2026-03-13: this section will eventually cover io_uring wrappers too via IO-CALL
(defmacro io-syscall ((name &rest args) &optional (success-form '(values io-result io-error)))
  "Wrap a syscall which is bound to alien-function NAME, passing it
ARGS. SUCCESS-FORM is returned when return-type is INT and result is >0,
defaulting to (values IO-RESULT IO-ERROR) which are lexically bound and
exposed by this macro."
  (let ((rtyp (syscall-return-type name)))
    `(locally
         (declare (optimize (sb-c::float-accuracy 0)))
       (sb-alien:set-errno 0)
       (let ((io-result (,name ,@args))
             (io-error (get-errno)))
         (if (,(syscall-error-predicate rtyp) io-result)
             (values nil io-error)
             ,success-form)))))

(defmacro io-syscall* ((name &rest args) &optional (success-form '(values io-result io-error)))
  "Like IO-SYSCALL but check and signal conditions based on the second
value of the syscall NAME, or return the first (actual) value."
  (with-gensyms (ret code)
    `(multiple-value-bind (,ret ,code) (io-syscall (,name ,@args) ,success-form)
       (if-let ((err (get-syscall-error ,code)))
         (error err)
         ,ret))))

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
           (+ (get-monotonic-time)
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

;;; FD Wait
(defun compute-poll-flags (type)
  (ecase type
    (:input  (logior sb-unix:pollin sys::epollrdhup sys::pollpri))
    (:output (logior sb-unix:pollout))
    (:io     (logior sb-unix:pollin sys::epollrdhup sys::pollpri sb-unix:pollout))))

(defun process-poll-revents (revents fd)
  (let ((readp nil) (writep nil))
    (flags-case revents
      ((sb-unix:pollin sys::epollrdhup sys::pollpri)
       (setf readp t))
      ((sb-unix:pollout sb-unix:pollhup)
       (setf writep t))
      ((sb-unix:pollerr)
       (error 'poll-error :fd fd))
      ((sb-unix:pollnval)
       (error 'poll-error :fd fd :type "Invalid file descriptor")))
    (values readp writep)))

(defun wait-until-fd-ready (file-descriptor event-type &optional timeout errorp)
  "Poll file descriptor `FILE-DESCRIPTOR' for I/O readiness.
`EVENT-TYPE' must be either :INPUT, :OUTPUT, or :IO.
`TIMEOUT' must be either a non-negative real measured in seconds,
or `NIL' meaning no timeout at all. If `ERRORP' is not NIL and a timeout
occurs, then a condition of type `POLL-TIMEOUT' is signaled.
Returns two boolean values indicating readability and writeability of `FILE-DESCRIPTOR'."
  (flet ((poll-error (unix-err)
           (error 'io/sys:poll-error :fd file-descriptor
                                     :type (io/sys::error-type unix-err))))
    (with-alien ((pollfd (sb-alien:struct sb-unix::pollfd)))
      ;; (bzero pollfd (isys:sizeof '(:struct pollfd)))
      (with-alien-slots (sb-unix::fd sb-unix::events sb-unix::revents) pollfd
        (setf sb-unix::fd file-descriptor
              sb-unix::events (compute-poll-flags event-type))
        (handler-case
            (let ((ret (io/sys::repeat-upon-condition-decreasing-timeout
                           ((io/sys::eintr) remaining-time timeout)
                         (sb-unix:unix-simple-poll pollfd event-type (io/sys::timeout-ms remaining-time)))))
              (when (zerop ret)
                (if errorp
                    (error 'poll-timeout :fd file-descriptor :event-type event-type)
                    (return-from wait-until-fd-ready (values nil nil)))))
          (io/sys::syscall-error (err) (poll-error err)))
        (process-poll-revents sb-unix::revents file-descriptor)))))

(defun fd-ready-p (fd &optional (event-type :input))
  "Tests file-descriptor `FD' for I/O readiness.
`EVENT-TYPE' must be either :INPUT, :OUTPUT or :IO ."
  (multiple-value-bind (readp writep)
      (wait-until-fd-ready fd event-type 0)
    (ecase event-type
      (:input  readp)
      (:output writep)
      (:io     (or readp writep)))))

(defun fd-readablep (fd)
  (nth-value 0 (wait-until-fd-ready fd :input 0)))

(defun fd-writablep (fd)
  (nth-value 1 (wait-until-fd-ready fd :output 0)))

;;; PAM
(define-condition pam-condition (sys-condition) ())
(define-condition pam-error (sys-condition std-error) 
  ((name :initarg :name :accessor error-name))
  (:report (lambda (c s) 
             (format s "PAM error ~S ~A" (error-name c) (security:pam-result (error-name c)))
             (when (error-message c) (format s ": ~A" (error-message c))))))

;; TODO 2026-04-05: define pam-errors

#+todo
(defun pam-unwrap (form)
  "Unwrap the result of a PAM foreign call in FORM, signaling conditions as
needed or returning :SUCCESS.")
