;;; sys.lisp --- Linux System IO

;; Syscalls

;;; Code:
(in-package :io/sys)

;;; Conditions
(define-condition sys-condition () ())
(define-condition sys-error (error sys-condition) ())
(define-condition syscall-error (sys-error std-error) ())
(define-condition poll-error (syscall-error)
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

(defun syscall-error (control-string &rest args)
  (error 'syscall-error :message (format nil "~?" control-string args)))

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
  `(repeat-decreasing-timeout (,timeout-var ,timeout ,block-name)
     (acase (progn ,@body)
       ,@(mapcar (lambda (x) `(,x (return-from ,block-name std::it))) ints))))

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
