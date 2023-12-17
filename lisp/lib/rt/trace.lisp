;;; lib/rt/trace.lisp --- Tracing Framework

;; This package provides utilities for tracing Lisp code and
;; displaying traces to the user. In addition to extending the
;; built-in TRACE macro and SB-DEBUG functionality we have a tracer
;; which works with Chrome's built-in tracer: chrome://trace.

;; The chrome tracer is a slightly modernized version of TeMPOraL's
;; work available here: https://github.com/TeMPOraL/tracer.

;; ref: https://www.chromium.org/developers/how-tos/trace-event-profiling-tool/

;; - sb-debug manual: https://www.sbcl.org/manual/#Debugger

;; - sb-debug notes: https://gist.github.com/nikodemus/659495

;;; Code:
(in-package :rt/trace)

(defmacro traced-flet (functions &body body)
  (flet ((add-traces (function) 
           (destructuring-bind 
               (name lambda-list &body body) function 
             `(,name ,lambda-list 
                     ,@(cons `(format t 
                                      "Calling ~a with ~@{~a=~a, ~}~%" 
                                      ',name 
                                      ,@(loop for symbol in lambda-list 
                                          collecting `(quote ,symbol) 
                                          collecting symbol)) 
                              body)))))
    `(flet ,(mapcar #'add-traces functions) ,@body)))

;;; This is an implementation of a chromium-based Lisp tracer authored
;;; by TeMPOraL. The source is available here:
;;; https://github.com/TeMPOraL/tracer/tree/master

(defvar *trace-events* nil "A list of trace entries, pushed onto from the beginning.")

(defvar *original-trace-start-breakpoint-fun* #'sb-debug::trace-start-breakpoint-fun "Original SBCL function being overwritten by the tracer.")
(defvar *original-trace-end-breakpoint-fun* #'sb-debug::trace-end-breakpoint-fun "Original SBCL function being overwritten by the tracer.")

(defvar *clock-reset-fun* nil)
(defvar *clock-get-time-fun* nil)

(defvar *trace-event-default-pid* 1 "The default value for PID for the trace events. This library is currently intended for use within a single process only.")

(defvar +arg-converter-ignore-all+ (constantly 'skipped) "A converter that rejects all parameters.")
(defvar +arg-converter-passthrough+ (lambda (&rest args) args) "A converter that remembers all args as is, without modifying them.")
(defvar +arg-converter-store-only-simple-objects+ (lambda (&rest args)
                                                    (mapcar (lambda (arg)
                                                              (typecase arg
                                                                ((or boolean character number symbol)
                                                                 arg)
                                                                (t
                                                                 (type-of arg))))
                                                            args))
  "A converter that remembers directly only objects of simple types, that cannot or are very unlikely to be destructively modified.")
(defvar +arg-converter-store-only-simple-objects-and-strings+ (lambda (&rest args)
                                                                (mapcar (lambda (arg)
                                                                          (typecase arg
                                                                            ((or boolean character number symbol string)
                                                                             arg)
                                                                            (t
                                                                             (type-of arg))))
                                                                        args))
  "Like `+ARG-CONVERTER-STORE-ONLY-SIMPLE-OBJECTS+', but also records strings as-is, hoping they don't get destructively modified too much.")

(defvar *default-arg-converter* +arg-converter-ignore-all+)
(defvar *tracing-arg-converters* (make-hash-table :test 'equal))



;;; The format of trace event; created primarily for reference, though later on we might upgrade to vector storage, and then it'll come helpful.
(defstruct (trace-event (:type list))
  "A single event being traced. "
  (phase :undefined :type keyword)
  (name nil :type (or symbol cons))
  (thread 0 :type t)
  (timestamp 0 :type fixnum)
  (args nil :type t)
  (duration 0 :type (or null (unsigned-byte 62)))
  (id nil :type t))

;;; TODO: define accessors manually, to save performance? or somehow optimize it.  -- Jacek Złydach, 2019-11-04

(declaim (inline convert-args))
(defun convert-args (traced-fn-name args)
  "Depending on the function being traced, named `TRACED-FN-NAME', and the value of `*DEFAULT-ARG-CONVERTER*'
convert the list of arguments `ARGS' to a form suitable for storing with the trace event, using a converter
registered under `*TRACING-ARG-CONVERTERS*'.
Returns the representation of `ARGS' to store."
  (declare (optimize (speed 3)))
  (apply (the function (gethash traced-fn-name *tracing-arg-converters* *default-arg-converter*))
         args))

(declaim (inline make-trace-event-fast))
(defun make-trace-event-fast (phase name thread timestamp args duration id)
  "Like `MAKE-TRACE-EVENT', but inlined, unsafe and without typechecking."
  (declare (optimize (speed 3)))
  (list phase name thread timestamp (convert-args name args) duration id))

;;; Timer

;;; TODO: make it so that user can plug a high-resolution timer here. -- Jacek Złydach, 2019-10-18

(sb-ext:defglobal *hack-clock-jitter* 0 "A crude hack because our clock has too little resolution.")
(declaim (type fixnum *hack-clock-jitter*))

;;; TODO: this needs to be a function that can be changed between invocations of tracing!
;;; I want to allow for injecting higher resolution clocks if available.
;;; -- Jacek Złydach, 2019-11-01

(defun get-current-time-usec ()
  "Get current time with microsecond resolution."
  (sb-ext:atomic-incf *hack-clock-jitter*)
  (+ (* (get-internal-real-time) 1000)
     *hack-clock-jitter*))

(declaim (ftype (function () (unsigned-byte 62)) get-current-time-usec)
         (inline get-current-time-usec))
(defun get-current-time-usec-nojitter ()
  "Get current time with microsecond resolution. No extra jitter involved."
  (declare (optimize (speed 3)))
  (the (unsigned-byte 62) (* (get-internal-real-time) 1000)))

;;; XXX: below is our new, usec clock -- Jacek Złydach, 2019-11-02
(let ((clock-offset 0))
  (declare (type (unsigned-byte 62) clock-offset))
  (defun %%start-clock ()
    (setf clock-offset (sb-kernel::get-time-of-day)))
  (defun %%get-time-usec ()
    (multiple-value-bind (sec usec)
        (sb-kernel::get-time-of-day)
      (+ (* (- sec clock-offset) 1000000) usec)))
  (defun %%time (thunk)
    (let ((start (%%get-time-usec)))
      (funcall thunk)
      (- (%%get-time-usec)  start)))
  (setf *clock-reset-fun* #'%%start-clock
        *clock-get-time-fun* #'%%get-time-usec))

(declaim (ftype (function () (values (unsigned-byte 62) &optional)) get-current-time)
         (inline get-current-time))
(defun get-current-time ()
  (funcall *clock-get-time-fun*))

(defun post-process-entries (entries &key correct-zero-duration)
  "Destructively modify `ENTRIES', making it more compact and, if `CORRECT-ZERO-DURATION' is T,
changing zero-length events to have 1us length, also modifying other times to offset for that.
`ENTRIES' is expected to be in order entries were added. The function maintain separate offsets per (process, thread) pair.
Returns a processed list, to replace old value `ENTRIES'. As additional values, returns the total accumulated clock offset,
and the stacks containing unclosed duration entries, keyed by thread."
  (let ((offset 0)
        (stacks (make-hash-table :test 'equal)))
    (dolist (entry entries entries)
      ;; Always update event time to account for clock offset.
      (incf (trace-event-timestamp entry) offset)

      ;; Match starting and ending events to offset clock in case of zero-length events, and to convert
      ;; matching pairs of Duration events into Complete events.
      (let ((entry-ht-id (cons 1 (trace-event-thread entry)))) ;1 is the currently supported PID
        (ecase (trace-event-phase entry)
          (:enter
           ;; Push the :enter entry to stack.
           (push entry (gethash entry-ht-id stacks)))
          (:exit
           (let ((begin-event (first (gethash entry-ht-id stacks))))
             (if (equalp (trace-event-name begin-event)
                         (trace-event-name entry))
                 (progn
                   ;; Actual post-processing happens here.
                   ;; If zero-length and correct-zero-duration is on, update close time and offset.
                   (when (and correct-zero-duration
                              (= (trace-event-timestamp begin-event)
                                 (trace-event-timestamp entry)))
                     (incf (trace-event-timestamp entry))
                     (incf offset))

                   ;; Convert task into complete task + counter
                   (setf (trace-event-phase begin-event) :complete
                         (trace-event-phase entry) :skip ;TODO: counters, if any, go here -- Jacek Złydach, 2019-11-04
                         (trace-event-duration begin-event) (- (trace-event-timestamp entry) (trace-event-timestamp begin-event))
                         (trace-event-args begin-event) `(:in ,(trace-event-args begin-event) :out ,(trace-event-args entry)))

                   ;; Pop the updated entry from stack.
                   (pop (gethash entry-ht-id stacks)))
                 (warn "Recorded entries misordered; expected ~A, got ~A." (trace-event-name begin-event) (trace-event-name entry))))))))
    ;; Go over the list again, and remove "skip" entries.
    (deletef entries :skip :key #'trace-event-phase)
    (values entries
            offset
            stacks)))

;;; Tracing process

(defun %trace-start-breakpoint-fun (info)
  (let (conditionp)
    (values
     (lambda (frame bpt &rest args)
       (declare (ignore bpt))
       (sb-debug::discard-invalid-entries frame)
       (let ((condition (sb-debug::trace-info-condition info))
             (wherein (sb-debug::trace-info-wherein info)))
         (setq conditionp
               (and (not sb-debug::*in-trace*)
                    (or (not condition)
                        (apply (cdr condition) frame args))
                    (or (not wherein)
                        (sb-debug::trace-wherein-p frame wherein nil)))))
       (when conditionp
         (when (sb-debug::trace-info-encapsulated info)
           (sb-ext:atomic-push (make-trace-event-fast :enter
                                                      (sb-debug::trace-info-what info)
                                                      sb-thread:*current-thread*
                                                      (get-current-time)
                                                      args
                                                      nil
                                                      nil)
                               *trace-events*))
         ;; TODO: perhaps remove this, it seems unneeded -- Jacek Złydach, 2019-11-05
         (with-standard-io-syntax
           (apply #'sb-debug::trace-maybe-break info (sb-debug::trace-info-break info) "before"
                  frame args))))
     (lambda (frame cookie)
       (declare (ignore frame))
       (push (cons cookie conditionp) sb-debug::*traced-entries*)))))

(declaim (ftype (function (sb-debug::trace-info) function) %trace-end-breakpoint-fun))
(defun %trace-end-breakpoint-fun (info)
  (lambda (frame bpt values cookie)
    (declare (ignore bpt))
    (unless (eq cookie (caar sb-debug::*traced-entries*))
      (setf sb-debug::*traced-entries*
            (member cookie sb-debug::*traced-entries* :key #'car)))

    (let ((entry (pop sb-debug::*traced-entries*)))
      (when (and (not (sb-debug::trace-info-untraced info))
                 (or (cdr entry)
                     (let ((cond (sb-debug::trace-info-condition-after info)))
                       (and cond (apply #'funcall (cdr cond) frame values)))))
        (sb-ext:atomic-push (make-trace-event-fast :exit
                                                   (sb-debug::trace-info-what info)
                                                   sb-thread:*current-thread*
                                                   (get-current-time)
                                                   values
                                                   nil
                                                   nil)
                            *trace-events*)

        (apply #'sb-debug::trace-maybe-break info (sb-debug::trace-info-break-after info) "after"
               frame values)))))

(defun install-tracing-overrides ()
  (sb-ext:unlock-package (find-package 'sb-debug))
  (setf (symbol-function 'sb-debug::trace-start-breakpoint-fun) #'%trace-start-breakpoint-fun
        (symbol-function 'sb-debug::trace-end-breakpoint-fun) #'%trace-end-breakpoint-fun)
  (sb-ext:lock-package (find-package 'sb-debug)))

(defun uninstall-tracing-overrides ()
  (sb-ext:unlock-package (find-package 'sb-debug))
  (setf (symbol-function 'sb-debug::trace-start-breakpoint-fun) *original-trace-start-breakpoint-fun*
        (symbol-function 'sb-debug::trace-end-breakpoint-fun) *original-trace-end-breakpoint-fun*)
  (sb-ext:lock-package (find-package 'sb-debug)))

;;; FIXME: This should not be a macro. -- Jacek Złydach, 2019-10-18
(defun start-tracing (&rest specs)
  (install-tracing-overrides)
  `(progn
     (trace :encapsulate t :methods t ,@specs)))

(defun stop-tracing ()
  (untrace)
  (uninstall-tracing-overrides)
  #+nil(setf *trace-events* (nreverse *trace-events*))
  (multiple-value-bind (events offset stacks)
      (post-process-entries (nreverse *trace-events*))
    (declare (ignore offset stacks))
    (setf *trace-events* events))
  ;; TODO: report offsets and stacks -- Jacek Złydach, 2019-11-05
  (values))

(defun reset-tracing ()
  (setf *trace-events* nil
        *hack-clock-jitter* 0))

(defun get-tracing-report-data ()
  *trace-events*)

;;; Trace operations:
;;; 1. Reset
;;; 2. Trace
;;; 2.5 snapshot tracing?
;;; 3. Stop tracing
;;; 4. Save report

(defvar *tracing-p* nil "Is currently tracing activity happening?")

;;; Trace info entry type, for function call
;;; - Timestamp
;;; - Function name
;;; - Function args maybe? (trace-with-args), on enter
;;; - Function return value, on exit
;;; - Beginning or ending
;;; - Thread ID



;;; This prints a representation of the return values delivered.
;;; First, this checks to see that cookie is at the top of
;;; *TRACED-ENTRIES*; if it is not, then we need to adjust this list
;;; to determine the correct indentation for output. We then check to
;;; see whether the function is still traced and that the condition
;;; succeeded before printing anything.

(defmacro with-tracing ((&rest specs) &body body)
  `(unwind-protect
        (progn
          (reset-tracing)
          (start-tracing ,@specs)
          (progn
            ,@body))
     (stop-tracing)))



;;; FIXME: this still has an SBCL dependency -- Jacek Złydach, 2019-10-18
(defun function-name->name-and-category (function-name)
  (etypecase function-name
    (symbol
     (values (symbol-name function-name) (package-name (symbol-package function-name))))
    (cons
     (ecase (first function-name)
       (setf
        (values (format nil "~S" function-name) (package-name (symbol-package (second function-name)))))
       ((method sb-pcl::combined-method)
        (values (remove #\Newline (format nil "~S" function-name))
                (if (consp (second function-name))
                    (package-name (symbol-package (second (second function-name))))
                    (package-name (symbol-package (second function-name))))))))))

(defgeneric post-process-arg (arg)
  (:method ((arg t))
    "Passthrough method."
    (or (ignore-errors
          (prin1-to-string arg))
        "!!Error printing argument!!"))
  (:documentation "A hook useful for changing the printed representation of input and return values."))

(defmethod post-process-arg ((arg sequence))
  (if (every (lambda (el)  (typep el 'number)) arg)
      (format nil "[~{~F~^, ~}]" (coerce arg 'list))
      (call-next-method)))

;;; FIXME: Something breaks if not collecting args, and :skip-args is NIL. Probably the getf in printing. -- Jacek Złydach, 2019-11-05
(defun trace-event->json (trace-event &key (skip-args nil))
  (flet ((sanitize-and-format-args-list (argslist)
           (if skip-args "\"skipped\""
               (substitute #\Space #\Newline (format nil "[~{~S~^, ~}]" (mapcar #'post-process-arg argslist))))))
    (ecase (trace-event-phase trace-event)
      (:enter
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"B\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"args\" : { \"in\" : ~A }}"
                 name
                 category
                 (sb-impl::get-lisp-obj-address (trace-event-thread trace-event))
                 (trace-event-timestamp trace-event)
                 (sanitize-and-format-args-list (trace-event-args trace-event)))))
      (:exit
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"E\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"args\" : { \"out\" : ~A }}"
                 name
                 category
                 (sb-impl::get-lisp-obj-address (trace-event-thread trace-event))
                 (trace-event-timestamp trace-event)
                 (sanitize-and-format-args-list (trace-event-args trace-event)))))
      (:complete
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"X\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"dur\" : ~D,  \"args\" : { \"in\" : ~A, \"out\" : ~A }}"
                 name
                 category
                 (sb-impl::get-lisp-obj-address (trace-event-thread trace-event))
                 (trace-event-timestamp trace-event)
                 (trace-event-duration trace-event)
                 (sanitize-and-format-args-list (getf (trace-event-args trace-event) :in))
                 (sanitize-and-format-args-list (getf (trace-event-args trace-event) :out))))))))

(defun thread->json (thread)
  (format nil
          "{ \"name\" : \"thread_name\", \"ph\" : \"M\", \"pid\" : 1, \"tid\" : ~D, \"args\" : { \"name\" : ~S }}"
          (sb-impl::get-lisp-obj-address thread)
          (sb-thread:thread-name thread)))

(defun extract-threads (events)
  (loop
     with uniques-ht = (make-hash-table :test #'eq)
     for event in events
     do
       (setf (gethash (trace-event-thread event) uniques-ht) t)
     finally
       (return (hash-table-keys uniques-ht))))

;;; FIXME: save with streams instead? -- Jacek Złydach, 2019-10-14
(defun save-report (output-file-name &key (skip-args t))
  (with-open-file (stream output-file-name :direction :output :if-exists :supersede)
    ;; TODO: preamble -- Jacek Złydach, 2019-10-14
    (format stream "{~%\"traceEvents\" :  [~%")
    (loop
       for (entry . restp) on *trace-events*
       do
         (write-string (trace-event->json entry :skip-args skip-args) stream)
         (when restp
           (write-string "," stream)
           (terpri stream)))
    (loop
       for (thread . restp) on (extract-threads *trace-events*)
       initially
         (write-string "," stream)
         (terpri stream)
       do
         (write-string (thread->json thread) stream)
         (when restp
           (write-string "," stream)
           (terpri stream)))

    (format stream "~&],
\"displayTimeUnit\" : \"ms\",
\"application\" : \"FIXME\",
\"version\" : \"FIXME\",
\"traceTime\" : ~S
}"
            " TODO local-time independent time"
            ;;(local-time:format-timestring nil (local-time:now))
            ))
  (values))



;;; Helper function for blacklisting symbols when tracing whole packages.
(defun package-symbols-except (name &rest exceptions)
  (let (symbols
        (package (sb-impl::find-undeleted-package-or-lose name)))
    (do-all-symbols (symbol (find-package name))
      (when (eql package (symbol-package symbol))
        (when (and (fboundp symbol)
                   (not (macro-function symbol))
                   (not (special-operator-p symbol)))
          (push symbol symbols))
        (let ((setf-name `(setf ,symbol)))
          (when (fboundp setf-name)
            (push setf-name symbols)))))
    (set-difference symbols exceptions :key (lambda (x)
                                              (if (consp x)
                                                  (string (second x))
                                                  (string x))) :test #'string-equal)))
