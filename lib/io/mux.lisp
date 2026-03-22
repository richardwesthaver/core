;;; mux.lisp --- Multiplexer

;; Based on IOLib (iomux)

;;; Code:
(in-package :io/mux)

(defvar *multiplexers* nil
  "A list of all available multiplexers.")

(defvar *default-multiplexer* 'epoll-multiplexer
  "The default multiplexer for the current machine.")

;;; File Descriptors
(deftype fd-event-type ()
  '(member :read :write))

(defun get-fd-limit ()
  "Return the maximum number of FDs available for the current process."
  (sys:rlimit sys::rlimit-nofile))

(defstruct (fd-handler
             (:constructor make-fd-handler
                           (fd type callback oneshot-p &optional timer))
             (:copier nil))
  (fd nil :type unsigned-byte)
  (type nil :type fd-event-type)
  (callback nil :type function-designator)
  (timer nil :type (or null io-timer))
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
  ;; (:method-combination progn :most-specific-last)
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

(defmethod close-multiplexer ((mux multiplexer))
  (when (and (slot-boundp mux 'fd) (not (null (fd mux))))
    (sb-posix:close (fd mux))
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
   (timers :initform (make-pqueue :key #'io/sys::%io-timer-expire-time)
           :reader timers)
   (fd-timers :initform (make-pqueue :key #'io/sys::%io-timer-expire-time)
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

(defgeneric set-io-handler (base fd event-type function &key &allow-other-keys))
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
  (close-multiplexer (mux base))
  (dolist (slot '(mux fds timers fd-timers expired-events))
    (setf (slot-value base slot) nil))
  (values base))

(defmacro with-event-base ((var &rest initargs) &body body)
  "Binds VAR to a new EVENT-BASE, instantiated with INITARGS,
within the extent of BODY.  Closes VAR."
  `(let ((,var (make-instance 'event-base ,@initargs)))
     (unwind-protect (progn ,@body)
       (when ,var (close ,var)))))

;;;; Event Loop
(defun fd-entry (event-base fd)
  (gethash fd (fds event-base)))

(defun (setf fd-entry) (fd-entry event-base fd)
  (setf (gethash fd (fds event-base)) fd-entry))

(defmethod exit-event-loop ((event-base event-base) &key (delay 0))
  (add-timer event-base
             (lambda () (setf (state event-base) :exit))
             delay :oneshot t))

(defmethod event-base-empty-p ((event-base event-base))
  (and (zerop (hash-table-count (fds event-base)))
       (pqueue-empty-p (timers event-base))))

(defmethod set-io-handler :before
    ((event-base event-base) fd type function &key timeout oneshot &allow-other-keys)
  (check-type fd unsigned-byte)
  (check-type type fd-event-type)
  (check-type function function-designator)
  (check-type timeout (or null real))
  (check-type oneshot boolean)
  (when (fd-monitored-p event-base fd type)
    (error "FD ~A is already monitored for event ~A" fd type)))

(defun fd-monitored-p (event-base fd event-type)
  "Generalised predicate returning the event handler if the given FD
is monitored for EVENT-TYPE."
  (let ((entry (fd-entry event-base fd)))
    (and entry (fd-entry-handler entry event-type))))

(defmethod set-io-handler ((event-base event-base) fd type function &key timeout oneshot)
  (let ((current-fd-entry (print (fd-entry event-base fd)))
        (event (print (make-fd-handler fd type function oneshot))))
    (cond
      (current-fd-entry
       (%set-io-handler event-base fd event current-fd-entry timeout)
       (update-fd (mux event-base) current-fd-entry type :add))
      (t
       (let ((new-fd-entry (make-fd-entry fd)))
         (%set-io-handler event-base fd event new-fd-entry timeout)
         (monitor-fd (mux event-base) new-fd-entry))))
    event))

(defun %set-io-handler (event-base fd event fd-entry timeout)
  (when timeout
    (%set-io-handler-timer event-base event timeout))
  (setf (fd-entry-handler fd-entry (fd-handler-type event)) event)
  (setf (fd-entry event-base fd) fd-entry)
  event)

(defun %set-io-handler-timer (event-base event timeout)
  (let ((timer (make-io-timer (lambda () (expire-event event-base event)) timeout)))
    (setf (fd-handler-timer event) timer)
    (schedule-io-timer (fd-timers event-base) timer)))

(defun expire-event (event-base event)
  (push event (expired-events event-base)))

(defmethod set-error-handler :before ((event-base event-base) fd function)
  (check-type fd unsigned-byte)
  (check-type function function-designator)
  (unless (fd-entry event-base fd)
    (error "FD ~A is not being monitored" fd))
  (when (fd-has-error-handler-p event-base fd)
    (error "FD ~A already has an error handler" fd)))

(defun fd-has-error-handler-p (event-base fd)
  (let ((entry (fd-entry event-base fd)))
    (and entry (fd-entry-error-callback entry))))

(defmethod set-error-handler
    ((event-base event-base) fd function)
  (let ((fd-entry (fd-entry event-base fd)))
    (setf (fd-entry-error-callback fd-entry) function)))

(defmethod add-timer :before ((event-base event-base) function timeout &key oneshot)
  (check-type function function-designator)
  (check-type timeout (or null real))
  (check-type oneshot boolean))

(defmethod add-timer ((event-base event-base) function timeout &key oneshot)
  (schedule-io-timer 
   (timers event-base)
   (make-io-timer function (when timeout (coercef timeout 'io/sys::timeout)) :oneshot oneshot)))

(defmethod remove-fd-handlers ((event-base event-base) fd &key read write error)
  (unless (or read write error)
    (setf read t write t error t))
  (let ((entry (fd-entry event-base fd)))
    (cond
      (entry
       (prog1
           (%remove-fd-handlers event-base fd entry read write error)
         (when (and read write)
           (assert (null (fd-entry event-base fd))))))
      (t nil))))

(defun %remove-fd-handlers (event-base fd entry read write error)
  (let ((rev (fd-entry-read-handler entry))
        (wev (fd-entry-write-handler entry))
        (eev (fd-entry-error-callback entry))
        (removed nil))
    (when (and rev read)
      (%remove-io-handler event-base fd entry rev)
      (setf removed t))
    (when (and wev write)
      (%remove-io-handler event-base fd entry wev)
      (setf removed t))
    (when (and eev error)
      (setf (fd-entry-error-callback entry) nil)
      (setf removed t))
    removed))

(defun %remove-io-handler (event-base fd fd-entry event)
  (let ((event-type (fd-handler-type event)))
    (setf (fd-entry-handler fd-entry event-type) nil)
    (when-let ((timer (fd-handler-timer event)))
      (unschedule-io-timer (fd-timers event-base) timer))
    (cond
      ((fd-entry-empty-p fd-entry)
       (%remove-fd-entry event-base fd)
       (unmonitor-fd (mux event-base) fd-entry))
      (t
       (update-fd (mux event-base) fd-entry event-type :del)))))

(defun %remove-fd-entry (event-base fd)
  (remhash fd (fds event-base)))

(defmethod remove-timer ((event-base event-base) (timer io-timer))
  (unschedule-io-timer (timers event-base) timer))

;;;; Event Dispatch
(defvar *minimum-event-loop-step* 0.0d0)
(defvar *maximum-event-loop-step* nil)

(defmethod event-dispatch :around
    ((event-base event-base) &key timeout oneshot min-step max-step)
  (declare (ignore oneshot min-step max-step))
  (setf (state event-base) nil)
  (let ((timer (when timeout (exit-event-loop event-base :delay timeout))))
    (unwind-protect
         (call-next-method)
      (when timer
        (remove-timer event-base timer)))))

(defmethod event-dispatch ((event-base event-base) &key oneshot timeout
                           (min-step *minimum-event-loop-step*)
                           (max-step *maximum-event-loop-step*))
  (declare (ignore timeout))
  (coercef min-step 'double-float)
  (when max-step (coercef max-step 'double-float))
  (with-accessors ((mux mux) (fds fds) (state state)
                   (exit-when-empty exit-when-empty-p)
                   (timers timers) (fd-timers fd-timers)
                   (expired-events expired-events))
      event-base
    (labels ((poll-timeout (now)
               (let* ((deadline1 (time-to-next-timer timers))
                      (deadline2 (time-to-next-timer fd-timers))
                      (deadline (if (and deadline1 deadline2)
                                    (min deadline1 deadline2)
                                    (or deadline1 deadline2))))
                 (if deadline
                     (clamp-timeout (- deadline now) min-step max-step)
                     max-step)))
             (must-exit-loop-p ()
               (or state
                   (and exit-when-empty (event-base-empty-p event-base)))))
      (loop with deletion-list = ()
            with eventsp = nil
            for now = (get-internal-real-time)
            for poll-timeout = (poll-timeout now)
            until (must-exit-loop-p)
            do (setf expired-events nil)
               (setf (values eventsp deletion-list)
                     ;; todo
                     (dispatch-fd-events-once event-base poll-timeout now))
               (%remove-handlers event-base (delete nil deletion-list))
               (when (expire-pending-timers fd-timers now) (setf eventsp t))
               (dispatch-fd-timeouts expired-events)
               (when (expire-pending-timers timers now) (setf eventsp t))
               (when (and eventsp oneshot) (setf state :oneshot))))))

(defun %remove-handlers (event-base event-list)
  (loop :for ev :in event-list
        :for fd := (fd-handler-fd ev)
        :for fd-entry := (fd-entry event-base fd)
     :do (%remove-io-handler event-base fd fd-entry ev)))

;;; Waits for events and dispatches them.  Returns T if some events
;;; have been received, NIL otherwise.
(defun dispatch-fd-events-once (event-base timeout now)
  ;; (mumble "dispatching fd events..")
  (let ((wthreshold (write-interval-threshold event-base)))
    (loop
      with fd-events = (harvest-events (mux event-base) timeout) ; NIL
      for ev in fd-events
      for dlist = (%handle-one-fd event-base ev now nil wthreshold) ; #()
      then (%handle-one-fd event-base ev now dlist wthreshold)
      finally (pqueue-reorder (fd-timers event-base))
              (return (values (consp fd-events) dlist)))))

(defun %handle-one-fd (event-base event now deletion-list wthreshold)
  ;; (mumble "handling event: ~A" event)
  (destructuring-bind (fd ev-types) event
    (let* ((readp nil) (writep nil)
           (fd-entry (fd-entry event-base fd))
           (errorp (and fd-entry (member :error ev-types))))
      (when fd-entry
        (when (member :read ev-types)
          (setf readp (%dispatch-event fd-entry :read
                                       (if errorp :error nil) now)))
        (when (member :write ev-types)
          (when (<= wthreshold (- now (fd-entry-write-ts fd-entry)))
            (unwind-protect
                 (setf writep (%dispatch-event fd-entry :write
                                               (if errorp :error nil) now))
              (setf (fd-entry-write-ts fd-entry) now))))
        (when errorp
          (when-let ((callback (fd-entry-error-callback fd-entry)))
            (funcall callback (fd-entry-fd fd-entry) :error))
          (setf readp t writep t))
        (when readp (push (fd-entry-read-handler fd-entry) deletion-list))
        (when writep (push (fd-entry-write-handler fd-entry) deletion-list)))
      (values deletion-list))))

(defun %dispatch-event (fd-entry event-type errorp now)
  (let ((ev (fd-entry-handler fd-entry event-type)))
    (when ev
      (funcall (fd-handler-callback ev)
               (fd-entry-fd fd-entry)
               event-type
               (if errorp :error nil))
      (when-let ((timer (fd-handler-timer ev)))
        (reschedule-timer-relative-to-now timer now))
      (fd-handler-oneshot-p ev))))

(defun dispatch-fd-timeouts (events)
  (dolist (ev events)
    (funcall (fd-handler-callback ev)
             (fd-handler-fd ev)
             (fd-handler-type ev)
             :timeout)))

;;; EPOLL
;; preferred interface
(define-multiplexer epoll-multiplexer (multiplexer)
  ((events :reader events)))

(defmethod print-object ((mux epoll-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "epoll(4) multiplexer")))

(defmethod initialize-instance :after ((mux epoll-multiplexer) &key (size 25))
  (setf (slot-value mux 'fd) (io-syscall* (sys:epoll-create size)))
  (setf (slot-value mux 'events) (foreign-alloc 'sys:epoll-event :count (fd-limit mux))))

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
    (with-foreign-object (ev 'sys::epoll-event)
      ;; (bzero ev (alien-size sys:epoll-event))
      (setf (slot (sap-alien ev sys::epoll-event) 'sys::events)
            flags)
      (setf (slot
             (slot (sap-alien ev sys::epoll-event) 'sys::data)
             'sys::fd)
            fd)
      (handler-case (io-syscall* (sys:epoll-ctl (fd mux) sys::epoll-ctl-add fd ev))
        (io/sys::ebadf () (warn "FD ~A is invalid, cannot monitor it." fd))
        (io/sys::eexist () (warn "FD ~A is already monitored." fd))))))

(defmethod update-fd ((mux epoll-multiplexer) fd-entry event-type edge-change)
  (declare (ignore event-type edge-change))
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev 'sys:epoll-event)
      ;; (bzero ev (alien-size sys:epoll-event))
      (setf (slot (sap-alien ev sys:epoll-event) 'sys::events) flags)
      (setf (slot (slot (sap-alien ev sys:epoll-event) 'sys::data) 'sys::fd) fd)
      (handler-case (io-syscall* (sys:epoll-ctl (fd mux) sys::epoll-ctl-mod fd ev))
        (io/sys::ebadf () (warn "FD ~A is invalid, cannot update its status." fd))
        (io/sys::enoent () (warn "FD ~A was not monitored, cannot update its status." fd))))
    (values fd-entry)))

(defmethod unmonitor-fd ((mux epoll-multiplexer) fd-entry)
  (handler-case
      (io-syscall* (sys:epoll-ctl 
                   (fd mux)
                   sys::epoll-ctl-del
                   (fd-entry-fd fd-entry)
                   (null-pointer)))
    (io/sys::ebadf () (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (io/sys::enoent () (warn "FD ~A was not monitored, cannot unmonitor it." (fd-entry-fd fd-entry)))))

(defmethod harvest-events ((mux epoll-multiplexer) timeout)
  ;; (mumble "harvesting events with timeout: ~A" timeout)
  (with-accessors ((events events) (fd-limit fd-limit)) mux
    ;; REVIEW 2026-03-18: do we need to zero out the events pointer? causes malloc errors in current state
    ;; (bzero events (* fd-limit (alien-size sys:epoll-event)))
    (let (ready-fds)
      (repeat-upon-condition-decreasing-timeout ((io/sys::eintr) tmp-timeout timeout)
        (setf ready-fds (io-syscall* (sys:epoll-wait (fd mux) events fd-limit
                                                     (timeout-ms tmp-timeout)))))
      (macrolet ((epoll-slot (slot-name)
                   `(slot (sap-ref events 'sys:epoll-event i) ',slot-name)))
        ;; return* ? need to return from a specific block here, not the harvester
        (return-from harvest-events
          (loop for i below ready-fds
                for fd = (slot (epoll-slot sys::data) 'sys::fd)
                for event-mask = (epoll-slot sys::events)
                for epoll-event = (make-epoll-event fd event-mask)
                when epoll-event collect epoll-event))))))

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
