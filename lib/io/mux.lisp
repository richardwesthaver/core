;;; mux.lisp --- Multiplexer

;; Based on IOLib (iomux)

;;; Code:
(in-package :io/mux)

(defvar *multiplexers* nil
  "A list of all available multiplexers.")

(defvar *default-multiplexer* nil
  "The default multiplexer for the current machine.")

(defvar *multiplexer-order* nil
  "An ordered list of multiplexers to prioritize. Higher priority items come first.")

(defun get-fd-limit ()
  "Return the maximum number of FDs available for the current process."
  (let ((fd-limit (sys:rlimit sys::rlimit-nofile)))
    (if (= fd-limit sys::rlim-infinity)
        65536 ; 64K should be enough for anybody
        fd-limit)))

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

(defclass multiplexer ()
  ((fd :reader fd)
   (fd-limit :initform (get-fd-limit)
             :initarg :fd-limit
             :reader fd-limit)
   (closedp :accessor multiplexer-closedp
            :initform nil))
  (:documentation "Base class for I/O multiplexers."))

(defmethod close-multiplexer :around ((mux multiplexer))
  (unless (multiplexer-closedp mux)
    (call-next-method)
    (setf (multiplexer-closedp mux) t)))

#+todo
(defmethod close-multiplexer :progn ((mux multiplexer))
  (when (and (slot-boundp mux 'fd) (not (null (fd mux))))
    (close (fd mux))
    (setf (slot-value mux 'fd) nil))
  (values mux))

;; requires fd-entry
#+todo
(defmethod monitor-fd :before ((mux multiplexer) fd-entry)
  (with-accessors ((fd-limit fd-limit)) mux
    (let ((fd (fd-entry-fd fd-entry)))
      (when (and fd-limit (> fd fd-limit))
        (error "Cannot add such a large FD: ~A" fd)))))

(defmacro define-multiplexer (name priority superclasses slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,slots ,@options)
     (pushnew (cons ,priority ',name) *multiplexers*
              :test #'equal)))

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

(defun set-io-handler (base fd &rest args))
(defun event-dispatch (base &rest args))
(defgeneric set-error-handler (base fd function))
(defgeneric add-timer (event-base function timeout &key one-shot))
(defgeneric remove-fd-handlers (base fd &key read write error)
  (:documentation "Removes FD handlers for the given event types.
If READ, WRITE and ERROR are all NIL (the default), then all are removed.
Returns T if some handlers were removed, NIL otherwise."))
(defgeneric remove-timer (base timer))
(defgeneric event-dispatch (base &key one-shot timeout min-step max-step))
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
