(in-package :log)

(eval-always
  (defparameter *log-levels* (list nil :fatal :error :warn :info :debug :trace t)))

(deftype log-level-designator () `(member ,@*log-levels*))

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

(defvar *log-router* nil)

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
  
;; the purpose of this struct is to route log messages to the
;; appropriate output stream. It should be configured and bound to
;; *LOG-ROUTER*.
(defclass log-router ()
  ((fatal :initarg :fatal)
   (error :initarg :error)
   (warn :initarg :warn)
   (info :initarg :info)
   (debug :initarg :debug) 
   (trace :initarg :trace))
  (:default-initargs
   :fatal *error-output*
   :error *error-output*
   :warn *debug-io*
   :info *terminal-io* 
   :debug *debug-io*
   :trace *trace-output*))

(defmacro define-log-level (name &body pred)
  "Define a log-level of NAME with PRED being the body of the predicate
function 'NAME-P'."
  (let ((%name (string-upcase name)))
    `(progn
       (defun ,(intern (concatenate 'string %name "-P")) ()
         ,@(or pred `((eql *log-level* ,(sb-int:keywordicate name)))))
       (defun ,(intern (concatenate 'string %name "!")) (&rest args)
         (when (,(symbolicate (concatenate 'string %name "-P")))
           (format *trace-output* "#:~(~A~) ~@[~f~]~&"
                 ',name
                 (when *log-timestamp* (log-timestamp-source)))
           (mapc (lambda (x) (format *trace-output* "; ~A~&" x)) args))
         (if (= 1 (length args))
             (car args)
             args))
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
   :timestamp (time:now)
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

(defmethod format-message ((stream stream) (message simple-log-message))
  (format stream "~a [~4,a] ~{<~a>~}: ~a"
          (time:format-timestring nil (timestamp message) :format *log-timestamp-format*)
          (level message)
          (tags message)
          (format-message nil (content message))))

;; (format-message *standard-output* (make-instance 'simple-log-message :content "hi" :tags '(:test)))

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
    (log-message level tags datum)))

(defclass rotating-file-sink (file-sink)
  ((interval :accessor interval)
   (last-rotation :initform 0 :accessor last-rotation)
   (template :initarg :template :accessor template))
  (:default-initargs
   :interval :daily
   :file nil))

(defmethod rotate ((obj rotating-file-sink) &optional new-file)
  (let ((time (setf (last-rotation obj) (get-universal-time))))
    (cond (new-file
           (setf (file obj) new-file))
          (t
           (multiple-value-bind (s m h dd mm yy) (decode-universal-time time)
             (setf (file obj)
                   (make-pathname :name (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[ ~a~]"
                                                yy mm dd h m s (pathname-name (template obj)))
                                  :defaults (template obj))))))
    (setf (last-rotation obj) time)))

(defmethod initialize-instance :after ((obj rotating-file-sink) &key interval)
  (setf (interval obj) interval)
  (rotate obj))

(defmethod (setf interval) (value (obj rotating-file-sink))
  (ecase value
    ((:hourly :daily :monthly :weekly)
     (setf (slot-value obj 'interval) value))))

(defmethod msg :before ((obj rotating-file-sink) msg)
  (let ((pre (last-rotation obj))
        (now (get-universal-time)))
    (when (multiple-value-bind (s m h dd mm yy dow) (decode-universal-time now)
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
      (rotate obj))))

(defclass level-filter (filter)
  ((level :initform *log-level* :accessor level))
  (:default-initargs
   :level :info))

(defmethod initialize-instance :after ((filter level-filter) &key level)
  (setf (level filter) level))

(defmethod (setf level) :before (level (filter level-filter))
  (unless (find level *log-levels*)
    (cl:error "~a is neither a level in *LEVELS*, nor an integer." level)))

(defmethod msg ((filter level-filter) (message message))
  (let ((level (level filter)))
    (when (<= (position level *log-levels*)
              (position (level message) *log-levels*))
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
