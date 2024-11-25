;;; log.lisp --- Log Objects

;;; Code:
(in-package :log)

(eval-always
  (defparameter *log-levels* (vector nil :fatal :error :warn :info :debug :trace t)))

(defun ilevel (name)
  (position name *log-levels*))
(define-setf-expander ilevel (new name)
  (setf (svref *log-levels* (ilevel name)) new))

(deftype log-level-designator () `(or (member ,@(coerce *log-levels* 'list)) integer))

(declaim (log-level-designator *log-level*))
(defparameter *log-level* :debug
  "Logging is performed dynamically based on this variable. When NIL,
logging is disabled, which is equivalent to a level of :FATAL. When T,
Logging is enabled for all levels, which is equivalent to :TRACE.")

(defvar *log-message-class* 'simple-log-message
  "The class of messages sent to the logger. May be a subclass of LOG-MESSAGE or
T which indicates that message data will be sent without generating a new
object.")

(defvar *logger* nil)
(defvar *log-timestamp* t 
  "If non-nil, print a timestamp with log output. The value may be a
function in which case it is used as the function value of
`log-timestamp-source', or a number which will be used as the input arg to GET-REAL-TIME-SINCE.")

(defvar *log-timestamp-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(declaim (fixnum *log-indent*))
(defvar *log-indent* 0
  "Level of indentation to apply to multi-line log messages.")

(defun get-real-time-since (n)
  "Return the numbers of seconds since a relative value offset N."
  (- (get-internal-real-time) n))

(defun init-log-timestamp ()
  (setq *log-timestamp* (get-internal-real-time)))

;; TODO 2023-09-20: (declaim (inline log-timestamp-source)) ;; this
;; probably shouldn't be inlined.. bench it
(defun log-timestamp-source ()
  (typecase *log-timestamp*
    (function (funcall *log-timestamp*))
    (number (/ (get-real-time-since *log-timestamp*) #.internal-time-units-per-second))
    (t (/ (get-internal-real-time) #.internal-time-units-per-second))))

(defun universal-timestamp () (get-universal-time))
  
;; the purpose of this struct is to route log messages to the appropriate
;; output stream.
(defstruct log-router
  (fatal *error-output*)
  (error *error-output*)
  (warn *debug-io*)
  (info *terminal-io*)
  (debug *debug-io*)
  (trace *trace-output*))

(defmacro define-log-level (name &body pred)
  "Define a log-level of NAME with PRED being the body of the predicate
function 'NAME-P'."
  (let ((%name (string-upcase name)))
    `(progn
       (defun ,(intern (concatenate 'string %name "-P")) ()
         ,@(or pred `((eql *log-level* ,(sb-int:keywordicate name)))))
       (defun ,(intern (concatenate 'string %name "!")) (&rest args)
         (when (,(symbolicate (concatenate 'string %name "-P")))
           (fresh-line *trace-output*)
           (format *trace-output* "#:~(~A~)~@[ ~f~]~&"
                   ',name
                   (when *log-timestamp* (log-timestamp-source)))
           (if-let ((fmt (and (stringp (car args)) (pop args))))
             (apply 'format *trace-output* fmt args)
             (mapc (lambda (x) (format *trace-output* "; ~A~&" x)) args))
           (case (length args)
             (0 (values))
             (1 (car args))
             (t args))))
       (defun ,(intern (concatenate 'string %name "-DESCRIBE")) (&rest args)
         (,(intern (concatenate 'string %name "!")) (apply #'describe args))))))

(define-log-level trace (or (eql *log-level* :trace) (eql *log-level* t)))
(define-log-level debug (or (trace-p) (eql *log-level* :debug)))
(define-log-level info (or (debug-p) (eql *log-level* :info)))
(define-log-level warn (or (info-p) (eql *log-level* :warn)))
(define-log-level error (or (warn-p) (eql *log-level* :error)))
(define-log-level fatal t) ;; probably needs to be a special case

;; TODO 2023-08-31: single format control string
;; (defun debug! (&rest args)
;;   (when (debug-p)
;;     ;...
;;     ;; RESEARCH 2023-08-31: what's better here.. loop, do, mapc+nil?
;;     (map nil (lambda (x) (format t "~X~%" x)) args))
;;   args)

;;; Pipes
(defclass log-message (message) 
  ((timestamp :initarg :timestamp :accessor timestamp)
   (level :initarg :level :accessor level)
   (content :initarg :content :accessor content))
  (:default-initargs
   :timestamp (now)
   :level :info
   :content nil))

(defclass simple-log-message (log-message)
  ((thread :initarg :thread :accessor message-thread)
   (tags :initarg :tags :accessor tags))
  (:default-initargs
   :thread *current-thread*
   :tags nil))

(defmethod initialize-instance :before ((message log-message) &key level)
  (unless (typep level 'log-level-designator)
    (error "Level must be one of ~a" *log-levels*)))

(defmethod initialize-instance :before ((message simple-log-message) &key tags)
  (unless (every #'keywordp tags)
    (error "Tags must be keywords")))

(defvar *simple-log-message-formatter* (formatter "~a [~4,a] ~{<~a>~}: ~a"))

(defmethod format-message (stream (message simple-log-message))
  (format stream *simple-log-message-formatter*
          (format-timestring nil (timestamp message) :format *log-timestamp-format*)
          (level message)
          (tags message)
          (format-message nil (content message))))

(declaim (inline %log-object))
(defun %log-object (obj)
  (when *logger*
    (msg *logger* obj)))

(defun log-message (level tags content &optional (class *log-message-class*) &rest initargs)
  (unless (listp tags)
    (setf tags (list tags)))
  (%log-object (apply #'make-instance class :level level :tags tags :content content initargs)))

(defgeneric log-object (level tags datum &rest args)
  (:method (level tags (datum string) &rest args)
    (log-message level tags (apply #'format nil datum args)))
  (:method (level tags (datum symbol) &rest args)
    (log-object level tags (apply (if (subtypep datum 'condition)
                               #'make-condition
                               #'make-instance)
                           datum args)))
  (:method (level tags (datum function) &rest args)
    (log-message level tags (lambda () (apply datum args))))
  (:method (level tags datum &rest args)
    (declare (ignore args))
    (log-message level tags datum))
  (:method (level tags (datum condition) &rest args)
    (declare (ignore args))
    (log-message level tags (princ-to-string datum) 
                 'condition-message :condition datum)))

(defclass rotating-file-sink (file-sink)
  ((interval :accessor interval)
   (last-rotation :initform 0 :accessor last-rotation)
   (path :initarg :path :initform nil :accessor path))
  (:default-initargs
   :interval :daily
   :file nil))

(defmethod log-rotate ((obj rotating-file-sink) &optional new-file)
  (let ((time (setf (last-rotation obj) (get-universal-time))))
    (cond (new-file
           (setf (file obj) new-file))
          (t
           (multiple-value-bind (s m h dd mm yy) (decode-universal-time time)
             (setf (file obj)
                   (make-pathname :name (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[ ~a~]"
                                                yy mm dd h m s (pathname-name (path obj)))
                                  :defaults (path obj))))))
    (setf (last-rotation obj) time)))

(defmethod initialize-instance :after ((obj rotating-file-sink) &key interval)
  (setf (interval obj) interval)
  (log-rotate obj))

(defmethod (setf interval) (value (obj rotating-file-sink))
  (ecase value
    ((:hourly :daily :monthly :weekly)
     (setf (slot-value obj 'interval) value))))

(defmethod msg :before ((obj rotating-file-sink) msg)
  (let ((pre (last-rotation obj))
        (now (get-universal-time)))
    (when 
        (multiple-value-bind (s m h dd mm yy dow) (decode-universal-time now)
          (declare (ignore s m dow))
          (multiple-value-bind (ps pm ph pdd pmm pyy pdow) (decode-universal-time pre)
            (declare (ignore ps pm pdow))
            (ecase (interval obj)
              (:hourly
               (or (/= ph h) (/= pdd dd) (/= pmm mm) (/= pyy yy)))
              (:daily
               (or (/= pdd dd) (/= pmm mm) (/= pyy yy)))
              (:monthly
               (or (/= pmm mm) (/= pyy yy)))
              (:weekly
               (< (* 60 60 24 7) (- (get-universal-time) (last-rotation obj)))))))
      (log-rotate obj))))

(defclass level-filter (filter)
  ((level :initform *log-level* :accessor level))
  (:default-initargs
   :level :info))

(defmethod initialize-instance :after ((filter level-filter) &key level)
  (setf (level filter) level))

(defmethod (setf level) :before (level (filter level-filter))
  (unless (find level *log-levels*)
    (error 'invalid-argument :reason "LEVEL is not a member of *LEVELS* or an integer" :item level)))

(defmethod msg ((filter level-filter) (message message))
  (let ((level (level filter)))
    (when (<= (ilevel level)
              (ilevel (level message)))
      message)))

(defclass tag-filter (filter)
  ((tags :initarg :tags :initform t :accessor tags)))

(defmethod msg ((filter tag-filter) (message message))
  (when (or (eql (tags filter) T)
            (loop for tag in (tags filter)
                  thereis (find tag (tags message))))
    message))

(defclass tag-tree-filter (tag-filter) ())

(defvar *tag-separator* #\.)

(defun matching-tree-tag (filter tag)
  (let ((tag-leaves (ssplit *tag-separator* (string-upcase tag)))
        (filter-leaves (ssplit *tag-separator* (string-upcase filter))))
    (loop for ta in tag-leaves
       for fill in filter-leaves
       do (cond
            ((or (string= ta "*")
                 (string= fill "*"))
             (return t))
            ((not (string= ta fill))
             (return nil)))
       finally (return (>= (length tag-leaves)
                           (length filter-leaves))))))

(defmethod msg ((filter tag-tree-filter) (message message))
  (when (or (eql (tags filter) t)
            (loop for tag in (tags filter)
                  thereis (find tag (tags message) :test #'matching-tree-tag)))
    message))

;;; Log Sync
(defun log-sync (&optional (logger *logger*))
  (when (and logger (log-thread logger)
             (thread-alive-p (log-thread logger)))
    (with-sync-message sync
      (msg logger sync))))

;;; Logger
;; same as VERBOSE:CONTROLLER
(defclass logger (pipe)
  ((thread :initform nil :accessor log-thread)
   (thread-continue :initform nil :accessor log-thread-continue)
   (queue :initform (make-array '(10) :adjustable T :fill-pointer 0) :accessor queue)
   (queue-back :initform (make-array '(10) :adjustable T :fill-pointer 0) :accessor queue-back)
   (queue-condition :initform (make-waitqueue :name "message-condition") :reader queue-condition)
   (queue-lock :initform (make-mutex :name "message-lock") :reader queue-lock))
  (:documentation "A class which implements logging functionality. An instance of this class may
be designated as the 'global' logger by setting the value of *LOGGER*, or may
be implemented for a specific application."))

(defmethod print-object ((self logger) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~@[:threaded ~* ~]~@[:running ~* ~]:size ~d"
            (log-thread self) (log-thread-continue self) (length (queue self)))))

(defmethod start ((self logger))
  (setf (log-thread-continue self) t)
  (when (log-thread self)
    (cerror "Spawn a new thread anyway"
            "There is already a thread set on the logger."))
  (setf (log-thread self)
        (make-thread 
         (let ((*logger* self)
               (*standard-output* *standard-output*)
               (*error-output* *error-output*)
               (*trace-output* *trace-output*)
               (*query-io* *query-io*)
               (*debug-io* *debug-io*))
           (lambda () (logger-loop self)))
         :name "log-thread")))

(defmethod started-p ((self logger)) (log-thread-continue self))

(defmethod stopped-p ((self logger)) (not (log-thread-continue self)))

(defmethod stop ((self logger) &key)
  (setf (log-thread-continue self) nil)
  (loop for th = (log-thread self)
        for i from 0
        while (and th (thread-alive-p th))
        do (condition-notify (queue-condition self))
           (sleep 0.1)
           (when (< 5 i)
             (terminate-thread th)
             (return)))
  self)

(defmacro with-logger-lock ((&optional (logger '*logger*)) &body body)
  `(with-mutex ((queue-lock ,logger))
     ,@body))

(defmacro with-logger (logger &body body)
  "Temporarily bind LOGGER to *LOGGER* for the duration of BODY."
  `(let ((*logger* ,logger))
     ,@body))

(defmethod logger-loop ((self logger))
  (let* ((lock (queue-lock self))
         (condition (queue-condition self))
         (pipe (pipe self)))
    (grab-mutex lock)
    (unwind-protect
         (loop do (let ((queue (queue self)))
                    (rotatef (queue self) (queue-back self))
                    (release-mutex lock)
                    (with-simple-restart (skip "Skip processing this message batch.")
                      (loop for i from 0
                            for m across queue
                            do (with-simple-restart (continue "Continue processing messages, skipping ~A" m)
                                 (msg pipe m))
                               (setf (aref queue i) 0)))
                    (setf (fill-pointer queue) 0))
                  (grab-mutex lock)
                  (loop while (= 0 (length (queue self)))
                        do (condition-wait* condition lock :timeout 1))
               while (log-thread-continue self))
      (setf (log-thread self) nil)
      (ignore-errors (release-mutex lock)))))

(defmethod msg ((self logger) message)
  (let ((th (log-thread self)))
    (cond ((and th (thread-alive-p th)
                (not (eql *current-thread* th)))
           (with-logger-lock (self)
             (vector-push-extend message (queue self)))
           (condition-notify (queue-condition self)))
          (t (msg (pipe self) message))))
  nil)

;;; Commands
(defun add-pipe (&rest elements)
  (let ((logger (if (typep (first elements) 'logger)
                    (pop elements)
                    *logger*)))
    (with-logger-lock (logger)
      (let ((pipe (make-pipe)))
        (dolist (elt elements)
          (insert-element* elt pipe))
        (add-element logger pipe)))))

(defun default-logger (&rest args)
  (let ((pipe (apply 'make-instance 'logger args)))
    (defpipe (pipe)
      (level-filter :id 'repl-level)
      (tag-tree-filter :id 'repl-tags)
      (stream-sink :id 'repl-stream))))

(defun remove-logger ()
  (when *logger*
    (stop *logger*)
    (setf *logger* nil)))

(defun restart-logger ()
  (remove-logger)
  (setf *logger* (default-logger)))
