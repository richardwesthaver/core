;;; mux.lisp --- Multiplexer

;; Based on IOLib (iomux)

;;; Code:
(in-package :io/mux)

(defvar *multiplexers* nil
  "A list of all available multiplexers.")

(defvar *default-multiplexer* 'epoll-multiplexer
  "The default multiplexer for the current machine.")

(defvar *multiplexer-order* nil
  "An ordered list of multiplexers to prioritize. Higher priority items come first.")

;; TODO 2026-03-10: see if sbcl already does this
(defconstant +global-fd-limit+ 65536)

;;; File Descriptors
(deftype fd-event-type ()
  '(member :read :write))

(defun get-fd-limit ()
  "Return the maximum number of FDs available for the current process."
  (let ((fd-limit (sys:rlimit sys::rlimit-nofile)))
    (if (= fd-limit sys::rlim-infinity)
        +global-fd-limit+
        fd-limit)))

(defstruct (fd-handler
             (:constructor make-fd-handler
                           (fd type callback oneshot-p &optional timer))
             (:copier nil))
  (fd nil :type unsigned-byte)
  (type nil :type fd-event-type)
  (callback nil :type function-designator)
  (timer nil :type (or null sb-ext:timer))
  ;; oneshot events are removed after being triggered
  (oneshot-p nil :type boolean))

(defstruct (fd-entry
             (:constructor make-fd-entry (fd))
             (:copier nil))
  (fd 0 :type unsigned-byte)
  (read-handler  nil :type (or null fd-handler))
  (write-handler nil :type (or null fd-handler))
  (write-ts 0.0d0 :type double-float)
  (error-callback nil :type (or null function-designator)))

(defun fd-entry-handler (fd-entry event-type)
  (case event-type
    (:read  (fd-entry-read-handler  fd-entry))
    (:write (fd-entry-write-handler fd-entry))))

(defun (setf fd-entry-handler) (event fd-entry event-type)
  (case event-type
    (:read  (setf (fd-entry-read-handler  fd-entry) event))
    (:write (setf (fd-entry-write-handler fd-entry) event))))

(defun fd-entry-empty-p (fd-entry)
  (and (null (fd-entry-read-handler  fd-entry))
       (null (fd-entry-write-handler fd-entry))))

;;; Multiplexer
(defclass multiplexer ()
  ((fd :reader fd)
   (fd-limit :initform (get-fd-limit)
             :initarg :fd-limit
             :reader fd-limit)
   (closedp :accessor multiplexer-closedp
            :initform nil))
  (:documentation "Base class for I/O multiplexers."))

(defgeneric close-multiplexer (mux)
  (:method-combination progn :most-specific-last)
  (:documentation "Close multiplexer MUX, calling close() on the multiplexer's FD if bound."))

(defgeneric monitor-fd (mux fd-entry)
  (:documentation "Add the descriptor represented by FD-ENTRY to multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric update-fd (mux fd-entry event-type edge-change)
  (:documentation "Update the status of the descriptor represented by FD-ENTRY in multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric unmonitor-fd (mux fd-entry)
  (:documentation "Remove the descriptor represented by FD-ENTRY from multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric harvest-events (mux timeout)
  (:documentation "Wait for events on multiplexer MUX for a maximum time of TIMEOUT seconds.
Returns a list of fd/result pairs which have one of these forms:
  (fd (:read))
  (fd (:write))
  (fd (:read :write))
  (fd . :error)"))

(defmethod close-multiplexer :around ((mux multiplexer))
  (unless (multiplexer-closedp mux)
    (call-next-method)
    (setf (multiplexer-closedp mux) t)))

(defmethod close-multiplexer progn ((mux multiplexer))
  (when (and (slot-boundp mux 'fd) (not (null (fd mux))))
    (close (fd mux))
    (setf (slot-value mux 'fd) nil))
  (values mux))

(defmethod monitor-fd :before ((mux multiplexer) fd-entry)
  (with-accessors ((fd-limit fd-limit)) mux
    (let ((fd (fd-entry-fd fd-entry)))
      (when (and fd-limit (> fd fd-limit))
        (error "Cannot add such a large FD: ~A" fd)))))

(defmacro define-multiplexer (name superclasses slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,slots ,@options)
     (pushnew ',name *multiplexers*
              :test #'eql)))

;;; Events
(defclass event-base ()
  ((mux :reader mux)
   (fds :initform (make-hash-table :test 'eql)
        :reader fds)
   (timers :initform (make-pqueue :key #'sb-impl::%timer-expire-time)
           :reader timers)
   (fd-timers :initform (make-pqueue :key #'sb-impl::%timer-expire-time)
              :reader fd-timers)
   (expired-events :initform nil
                   :accessor expired-events)
   (write-interval-threshold :initarg :write-interval-threshold
                             :accessor write-interval-threshold)
   (state :initform nil :accessor state)
   (exit-when-empty :initarg :exit-when-empty :accessor exit-when-empty-p))
  (:default-initargs 
   :mux *default-multiplexer*
   :write-interval-threshold 0.0d0
   :exit-when-empty nil))

(defgeneric set-io-handler (base fd &rest args))
(defgeneric set-error-handler (base fd function))
(defgeneric add-timer (event-base function timeout &key oneshot))
(defgeneric remove-fd-handlers (base fd &key read write error)
  (:documentation "Removes FD handlers for the given event types.
If READ, WRITE and ERROR are all NIL (the default), then all are removed.
Returns T if some handlers were removed, NIL otherwise."))
(defgeneric remove-timer (base timer))
(defgeneric event-dispatch (base &key oneshot timeout min-step max-step))
(defgeneric exit-event-loop (base &key delay))
(defgeneric event-base-empty-p (base))

(defmethod initialize-instance :after
    ((base event-base) &key mux write-interval-threshold)
  (check-type write-interval-threshold non-negative-real)
  (setf (write-interval-threshold base)
        (float write-interval-threshold 1.0d0))
  (setf (slot-value base 'mux) (make-instance mux)))

(defmethod close ((base event-base) &key abort)
  (declare (ignore abort))
  (shutdown (mux base))
  (dolist (slot '(mux fds timers fd-timers expired-events))
    (setf (slot-value base slot) nil))
  (values base))

;;; EPOLL
;; preferred interface
(define-multiplexer epoll-multiplexer (multiplexer)
  ((events :reader events)))

(defmethod print-object ((mux epoll-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "epoll(4) multiplexer")))

(defmethod initialize-instance :after ((mux epoll-multiplexer) &key (size 25))
  (setf (slot-value mux 'fd) (sys:epoll-create size))
  (setf (slot-value mux 'events)
        (foreign-alloc 'sys:epoll-event
                       :count (fd-limit mux))))

(defmethod close :after ((mux epoll-multiplexer) &key abort)
  (declare (ignore abort))
  (with-slots (events) mux
    (when events
      (foreign-free events)
      (setf events nil))))

(defun calc-epoll-flags (fd-entry)
  (logior 
   (if (fd-entry-read-handler fd-entry)
       sys::epollin
       0)
   (if (fd-entry-write-handler fd-entry)
       sys::epollout
       0)
   sys::epollpri))

(defmethod monitor-fd ((mux epoll-multiplexer) fd-entry)
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-alien ((ev sys::epoll-event))
      (bzero ev (alien-size sys:epoll-event))
      (setf (slot ev 'sys::events)
            flags)
      (setf (slot
             (slot ev 'sys::data)
             'sys::fd)
            fd)
      (case (sys:epoll-ctl (fd mux) sys::epoll-ctl-add fd (addr ev))
        (sb-posix::ebadf (warn "FD ~A is invalid, cannot monitor it." fd))
        (sb-posix::eexist (warn "FD ~A is already monitored." fd))))))

(defmethod update-fd ((mux epoll-multiplexer) fd-entry event-type edge-change)
  (declare (ignore event-type edge-change))
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-alien ((ev sys:epoll-event))
      (bzero ev (alien-size sys:epoll-event))
      (setf (slot ev 'sys::events) flags)
      (setf (slot (slot ev 'sys::data) 'sys::fd) fd)
      (case (sys:epoll-ctl (fd mux) sys::epoll-ctl-mod fd (addr ev))
        (sb-posix:ebadf (warn "FD ~A is invalid, cannot update its status." fd))
        (sb-posix:enoent (warn "FD ~A was not monitored, cannot update its status." fd))))
    (values fd-entry)))

(defmethod unmonitor-fd ((mux epoll-multiplexer) fd-entry)
  (case
      (sys:epoll-ctl (fd mux)
                           sys::epoll-ctl-del
                           (fd-entry-fd fd-entry)
                           (null-pointer))
    (sb-posix:ebadf ()
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (sb-posix:enoent ()
     (warn "FD ~A was not monitored, cannot unmonitor it."
            (fd-entry-fd fd-entry)))))

;; TODO 2026-03-10: 
#+todo
(defmethod harvest-events ((mux epoll-multiplexer) timeout)
  (with-accessors ((events event-set-of)
                   (fd-limit fd-limit-of))
      mux
    (bzero events (* fd-limit (alien-size 'sys:epoll-event)))
    (let (ready-fds)
      (sys:repeat-upon-condition-decreasing-timeout
          ((sys::eintr) tmp-timeout timeout)
        (setf ready-fds (sys:epoll-wait (fd mux) events fd-limit
                                         (timeout->milliseconds tmp-timeout))))
      (macrolet ((epoll-slot (slot-name)
                   `(slot
                     ;; FIXME: tests fail when wrapping this bare reference
                     ;; in a :STRUCT.
                     (sap-ref events 'sys:epoll-event i)
                     'sys:epoll-event ',slot-name)))
        (return*
         (loop :for i :below ready-fds
               :for fd := (slot (epoll-slot sys::data) 'sys::fd)
               :for event-mask := (epoll-slot isys:events)
               :for epoll-event := (make-epoll-event fd event-mask)
               :when epoll-event :collect epoll-event))))))

(defun make-epoll-event (fd mask)
  (let ((event ()))
    (flags-case mask
      ((sys::epollout sys::epollhup)
       (push :write event))
      ((sys::epollin sys::epollpri sys::epollhup)
       (push :read event))
      (sys::epollerr
       (push :error event)))
    (when event
      (list fd event))))
