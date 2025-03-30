;;; threads.lisp --- Multi-thread utilities

;; Threading Macros

;;; Commentary:

;; mostly yoinked from sb-thread and friends

;;; Code:
(in-package :std/thread)

;; (sb-thread:thread-os-tid sb-thread:*current-thread*)
;; sb-thread:interrupt-thread

;;; Conditions
(define-condition std-thread-error (thread-error) ())

;;; Utils
(defun thread-support-p () (member :thread-support *features*))

(eval-always
  (defun print-top-level (msg)
    (let ((*standard-output* *standard-output*))
      (sb-thread:make-thread
       (lambda ()
         (format *standard-output* msg)))
    nil)))

(defun find-thread-by-id (id)
  "Search for thread by ID which must be an u64. On success returns the thread itself or nil."
  (find id (sb-thread::list-all-threads) :test '= :key 'thread-os-tid))

(defun find-thread (name)
  "Find a thread by name."
  (find name (sb-thread::list-all-threads) :test 'equal :key 'thread-name))

(defun thread-key-list ()
  (sb-thread::avltree-filter #'sb-thread::avlnode-key sb-thread::*all-threads*))

(defun thread-id-list ()
  (sb-thread::avltree-filter (lambda (th) (thread-os-tid (sb-thread::avlnode-data th))) sb-thread::*all-threads*))

(defun thread-count ()
  (sb-thread::avl-count sb-thread::*all-threads*))

(defun make-threads (n fn &key (name "thread"))
  (declare (type fixnum n))
  (loop for i below n
        collect (make-thread fn :name (format nil "~A-~D" name i))))

(defun parse-lambda-list-names (ll)
  (multiple-value-bind (idx _ args) (sb-int:parse-lambda-list ll)
    (declare (ignore idx _))
    (loop for a in args
          collect
             (etypecase a
               (atom a)
               (cons (car a))))))

(defmacro with-thread ((&key bindings name) &body body)
  `(let ((*default-special-bindings* ,bindings))
     (make-thread (lambda () ,@body)
                  :name ,name)))

(defmacro with-threads ((n &key args) &body body)
  `(make-threads ,n (lambda (,@args) (declare (ignorable ,@(parse-lambda-list-names args))) ,@body)))

(defun finish-threads (&rest threads)
  (let ((threads (flatten threads)))
    (unwind-protect
         (mapc #'join-thread threads)
      (dolist (thread threads)
        (when (thread-alive-p thread)
          (terminate-thread thread))))))

(defun timed-join-thread (thread timeout)
  (declare (type thread thread) (type float timeout))
  (handler-case (sb-sys:with-deadline (:seconds timeout)
                  (join-thread thread :default :aborted))
    (sb-ext:timeout ()
      :timeout)))

(defun hang ()
  (join-thread *current-thread*))

(defun kill-thread (thread)
  (when (thread-alive-p thread)
    (ignore-errors
      (terminate-thread thread))))

;; (sb-vm::primitive-object-slots (sb-vm::primitive-object 'sb-vm::thread))
(defun init-session (&optional (thread *current-thread*)) (sb-thread::new-session thread))

;; (sb-thread::with-progressive-timeout (timet :seconds 4) (dotimes (i 4000) (print (timet))))

;; (describe sb-thread::*session*)

;; make-listener-thread 

;; with-progressive-timeout

;; from sb-thread
(defun dump-thread ()
  (let* ((slots (sb-vm::primitive-object-slots #1=(sb-vm::primitive-object 'sb-vm::thread)))
         (sap (current-thread-sap))
         (thread-obj-len (sb-vm::primitive-object-length #1#))
         (names (make-array thread-obj-len :initial-element "")))
    (loop for slot across slots
          do
          (setf (aref names (sb-vm::slot-offset slot)) (sb-vm::slot-name slot)))
    (flet ((safely-read (sap offset &aux (bits (sb-vm::sap-ref-word sap offset)))
             (cond ((eql bits sb-vm:no-tls-value-marker) :no-tls-value)
                   ((eql (logand bits sb-vm:widetag-mask) sb-vm:unbound-marker-widetag) :unbound)
                   (t (sb-vm::sap-ref-lispobj sap offset))))
           (show (sym val)
             (declare (type fixnum sym))
             (let ((*print-right-margin* 128)
                   (*print-lines* 4))
               (format t " ~3d ~30a : ~s~%"
                       #+sb-thread (ash sym (- sb-vm:word-shift))
                       #-sb-thread 0
                       #+sb-thread (sb-vm:symbol-from-tls-index sym)
                       #-sb-thread sym
                       val))))
      (format t "~&TLS: (base=~x)~%" (sb-vm::sap-int sap))
      (loop for tlsindex from sb-vm:n-word-bytes below
            #+sb-thread (ash sb-vm::*free-tls-index* sb-vm:n-fixnum-tag-bits)
            #-sb-thread (ash thread-obj-len sb-vm:word-shift)
            by sb-vm:n-word-bytes
            do
         (unless (<= sb-vm::thread-allocator-histogram-slot
                     (ash tlsindex (- sb-vm:word-shift))
                     (1- sb-vm::thread-lisp-thread-slot))
           (let ((thread-slot-name
                  (if (< tlsindex (ash thread-obj-len sb-vm:word-shift))
                           (aref names (ash tlsindex (- sb-vm:word-shift))))))
                 (if (and thread-slot-name (sb-vm::neq thread-slot-name 'sb-vm::lisp-thread))
                     (format t " ~3d ~30a : #x~x~%" (ash tlsindex (- sb-vm:word-shift))
                             thread-slot-name (sb-vm::sap-ref-word sap tlsindex))
                     (let ((val (safely-read sap tlsindex)))
                       (unless (eq val :no-tls-value)
                         (show tlsindex val)))))))
      (let ((from (sb-vm::descriptor-sap sb-vm:*binding-stack-start*))
            (to (sb-vm::binding-stack-pointer-sap)))
        (format t "~%Binding stack: (depth ~d)~%"
                (/ (sb-vm::sap- to from) (* sb-vm:binding-size sb-vm:n-word-bytes)))
        (loop
          (when (sb-vm::sap>= from to) (return))
          (let ((val (safely-read from 0))
                (sym #+sb-thread (sb-vm::sap-ref-word from sb-vm:n-word-bytes) ; a TLS index
                     #-sb-thread (sb-vm::sap-ref-lispobj from sb-vm:n-word-bytes)))
            (show sym val))
          (setq from (sb-vm::sap+ from (* sb-vm:binding-size sb-vm:n-word-bytes))))))))

(defun wait-for-threads (threads)
  (map 'list (lambda (thread) (sb-thread:join-thread thread :default nil)) threads)
  (not (some #'sb-thread:thread-alive-p threads)))

(defun process-all-interrupts (&optional (thread sb-thread:*current-thread*))
  (sb-ext:wait-for (null (sb-thread::thread-interruptions thread))))

(defgeneric workers (self))

(defgeneric work (self &key &allow-other-keys))

(defgeneric lock (self))

(defclass thread-pool ()
  ((workers :initarg :workers :accessor workers)))

(defgeneric run-thread (self thunk &key name &allow-other-keys))

;; BORDEAUX-THREADS version
(defun condition-wait* (cvar lock &key timeout)
  (let ((success (condition-wait cvar lock :timeout timeout)))
    (when (not success)
      (grab-mutex lock))
    success))

(sb-ext:defglobal .known-threads-lock. (make-mutex :name "known-threads-lock"))
(sb-ext:defglobal .known-threads. (make-hash-table #-genera :weakness #-genera :key))

(defun %get-thread-wrapper (native-thread)
  (multiple-value-bind (thread presentp)
      (with-mutex (.known-threads-lock.)
        (gethash native-thread .known-threads.))
    (if presentp
        thread
        (error "Thread wrapper is supposed to exist for ~S"
               native-thread))))

(defun (setf thread-wrapper) (thread native-thread)
  (with-mutex (.known-threads-lock.)
    (setf (gethash native-thread .known-threads.) thread)))

(defun remove-thread-wrapper (native-thread)
  (with-mutex (.known-threads-lock.)
    (remhash native-thread .known-threads.)))

;; Forms are evaluated in the new thread or in the calling thread?
(defvar *default-special-bindings* nil
  "This variable holds an alist associating special variable symbols
  to forms to evaluate. Special variables named in this list will
  be locally bound in the new thread before it begins executing user code.

  This variable may be rebound around calls to MAKE-THREAD to
  add/alter default bindings. The effect of mutating this list is
  undefined, but earlier forms take precedence over later forms for
  the same symbol, so defaults may be overridden by consing to the
  head of the list.")

(macrolet
    ((defbindings (name docstring &body initforms)
         (check-type docstring string)
       `(std/macs:define-constant ,name
            (list
             ,@(loop for (special form) in initforms
                     collect `(cons ',special ',form)))
          :test #'equal
          :documentation ,docstring)))
  (defbindings +standard-io-bindings+
      "Standard bindings of printer/reader control variables as per
CL:WITH-STANDARD-IO-SYNTAX. Forms are evaluated in the calling thread."
    (*package*                   (find-package :common-lisp-user))
    (*print-array*               t)
    (*print-base*                10)
    (*print-case*                :upcase)
    (*print-circle*              nil)
    (*print-escape*              t)
    (*print-gensym*              t)
    (*print-length*              nil)
    (*print-level*               nil)
    (*print-lines*               nil)
    (*print-miser-width*         nil)
    ;; Genera doesn't yet implement COPY-PPRINT-DISPATCH
    ;; (Calling it signals an error)
    #-genera
    (*print-pprint-dispatch*     (copy-pprint-dispatch nil))
    (*print-pretty*              nil)
    (*print-radix*               nil)
    (*print-readably*            t)
    (*print-right-margin*        nil)
    (*random-state*              (make-random-state t))
    (*read-base*                 10)
    (*read-default-float-format* 'double-float)
    (*read-eval*                 nil)
    (*read-suppress*             nil)
    (*readtable*                 (copy-readtable nil))))

(defun compute-special-bindings (bindings)
  (remove-duplicates (append bindings +standard-io-bindings+)
                     :from-end t :key #'car))

(defvar *%current-thread*)

(defun establish-dynamic-env (thread function special-bindings trap-conditions)
  "Return a closure that binds the symbols in SPECIAL-BINDINGS and calls
FUNCTION."
  (let* ((bindings (compute-special-bindings special-bindings))
         (specials (mapcar #'car bindings))
         (values (mapcar (lambda (f) (eval (cdr f))) bindings)))
    (std/macs:named-lambda %establish-dynamic-env-wrapper ()
      (progv specials values
        (with-slots (%lock %return-values %exit-condition #+genera native-thread)
            thread
          (flet ((record-condition (c)
                   (with-mutex (%lock)
                     (setf %exit-condition c)))
                 (run-function ()
                   (let ((*%current-thread* nil))
                     ;; Wait until the thread creator has finished creating
                     ;; the wrapper.
                     (with-mutex (%lock)
                       (setf *%current-thread* (%get-thread-wrapper *%current-thread*)))
                     (let ((retval
                             (multiple-value-list (funcall function))))
                       (with-mutex (%lock)
                         (setf %return-values retval))
                       retval))))
            (unwind-protect
                 (if trap-conditions
                     (handler-case
                         (values-list (run-function))
                       (condition (c)
                         (record-condition c)))
                     (handler-bind
                         ((condition #'record-condition))
                       (values-list (run-function))))
              ;; Genera doesn't support weak key hash tables. If we don't remove
              ;; the native-thread object's entry from the hash table here, we'll
              ;; never be able to GC the native-thread after it terminates
              #+genera (remove-thread-wrapper native-thread))))))))

;; From Shinmera's VERBOSE
(defstruct sync-message
  (condition (make-waitqueue))
  (lock (make-mutex)))

(defmethod lock ((self sync-message)) (sync-message-lock self))
  
(defmethod msg ((vector vector) (msg sync-message))
  ;; ensure we're waiting on the condition..
  (with-mutex ((sync-message-lock msg)))
  (condition-notify (sync-message-condition msg)))

(defmacro with-sync-message (s &body body)
  `(let ((,s (make-sync-message)))
     (with-mutex ((sync-message-lock ,s))
       ,@body
       (condition-wait* (sync-message-condition ,s) (sync-message-lock ,s)))))
