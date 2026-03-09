;;; net/proto/swank.lisp --- Swank Protocol Support

;; The undocumented wire protocol of SLIME fame and fortune.

;;; Commentary:

;; ref: https://github.com/astine/swank-client/blob/master/swank-description.markdown

;; based on SWANK-CLIENT (Robert Brown <robert.brown@gmail.com>)

;;; Code:
(in-package :net/proto/swank)

;;; Vars
(defconstant +maximum-thread-count+ 1000)
(defvar *swank-thread-offset* 0)
(define-constant +abort+ (cons nil nil) :test 'equal
  :documentation "Unique object used to signal that a computation was aborted on the server.")
(defvar *swank-connections* '() "List of all open Swank connections.")
(defvar *swank-connections-lock* (make-mutex) "Lock protecting *SWANK-CONNECTIONS*.")
(defvar *default-swank-port* 4005)

;;; Conditions
(define-condition slime-network-error (error)
  ()
  (:documentation "Network problem while evaluating a form."))

;;; Connection
(defclass swank-connection (connection)
  ((host :reader host
         :type string
         :initarg :host
         :initform (required-argument :host)
         :documentation "Name of the host where the Swank server is running.")
   (port :reader port
         :type net/core::unprivileged-port
         :initarg :port
         :initform *default-swank-port*
         :documentation "Port number used to make a Swank server connection.")
   (socket :reader socket
           :type socket
           :initarg :socket
           :initform (required-argument :socket)
           :documentation "socket used to communicate with the Swank server.")
   (idx :reader idx
        :initform (incf *swank-thread-offset*)
        :type (integer 0 *)
        :documentation
        "All threads for this connection are presented to Emacs with this value added to
their thread ID.")
   (continuation-counter 
    :accessor continuation-counter
    :initform 0
    :type (integer 0 *)
    :documentation "Used to associate an ID with each evaluated form.")
   (rex-continuations 
    :accessor rex-continuations
    :initform '()
    :type list
    :documentation
    "List of (ID, continuation) pairs, one for each evaluation in progress. Used to
match each returned value with the continuation it should be passed to.")
   (state 
    :accessor state
    :initform :alive
    :type (member :alive :closing :dead)
    :documentation "State of the connection, either :ALIVE, :CLOSING, or :DEAD.")
   (connection-lock 
    :reader connection-lock
    :initform (make-mutex)
    :documentation "Lock protecting slots of this connection that are read and written by
concurrently running threads."))
  (:documentation "A connection to a Swank server."))

(defun add-open-connection (connection)
  "Adds CONNECTION to the set of open Swank connections."
  (with-mutex (*swank-connections-lock*)
    (push connection *swank-connections*)))

(defun remove-open-connection (connection)
  "Removes CONNECTION from the set of open Swank connections."
  (with-mutex (*swank-connections-lock*)
    (setf *swank-connections* (remove connection *swank-connections*))))

(defun find-connection-for-thread-id (thread-id)
  "Returns the open Swank connection associated with THREAD-ID."
  (with-mutex (*swank-connections-lock*)
    (let ((thread-offset (* (floor thread-id +maximum-thread-count+) +maximum-thread-count+)))
      (find thread-offset *swank-connections* :key #'idx))))

(defun server-thread-id (thread-id)
  "Maps the THREAD-ID in an event that must be forwarded to the thread ID known
by the remote Lisp to which it will be sent."
  (mod thread-id +maximum-thread-count+))

(defun forward-event-to-worker (form package thread-id id)
  "Determines whether an :emacs-rex event is intended for a remote worker Lisp
and if so forwards it.  When forwarding is successful, FORWARD-EVENT-TO-WORKER
returns T; otherwise, it returns NIL.

FORWARD-EVENT-TO-WORKER is called by code in Swank Crew's patch to Slime's
swank.lisp source file.  The forwarding it performs is used by Swank Crew
to handle debugging of conditions signalled on remote worker Lisps.  See
swank.lisp-patch in https://github.com/brown/swank-crew."
  (let ((connection (find-connection-for-thread-id thread-id)))
    (when connection
      (let ((remote-thread-id (server-thread-id thread-id)))
        (slime-send `(:emacs-rex ,form ,package ,remote-thread-id ,id) connection))
      t)))

(defvar *io-package*
  (let ((package (make-package :swank-io :use '())))
    (import '(nil t quote) package)
    package)
  "A package used by the Swank client code when printing s-expressions, so that
symbols in the printed output contain their package names.")

(defun slime-net-encode-length (n)
  "Encodes an integer as a 6-character, 24-bit hex string."
  (format nil "~6,'0,X" n))

(defun slime-net-send (sexp sock)
  "Sends SEXP to a Swank server over SOCK.  The s-expression is read and
evaluated by the remote Lisp."
  (let* ((payload (with-standard-io-syntax
                    (let ((*package* *io-package*))
                      (prin1-to-string sexp))))
         (utf8-payload (sb-ext:string-to-octets payload))
         ;; The payload always includes one more octet, an encoded newline character at the end.
         (payload-length (1+ (length utf8-payload)))
         (utf8-length (sb-ext:string-to-octets (slime-net-encode-length payload-length)))
         ;; The encoded length always takes 6 octets.
         (message (make-octets (+ (length utf8-length) payload-length))))
    (replace message utf8-length)
    (replace message utf8-payload :start1 (length utf8-length))
    (setf (aref message (1- (length message))) (char-code #\Newline))
    ;; We use IGNORE-ERRORS here to catch SB-INT:CLOSED-STREAM-ERROR on SBCL and any other
    ;; system-dependent network or stream errors.
    (let ((success (ignore-errors (write-sequence message (socket-make-stream sock :output t)))))
      (unless success (error 'slime-network-error)))))

(defun slime-send (sexp connection)
  "Sends SEXP to a Swank server using CONNECTION.  Signals SLIME-NETWORK-ERROR
if there are communications problems."
  (let ((sock (socket connection)))
    (slime-net-send sexp sock)
    ;; We use IGNORE-ERRORS here to catch SB-INT:CLOSED-STREAM-ERROR on SBCL and any other
    ;; system-dependent network or stream errors.
    (let ((success nil))
      (ignore-errors
       (progn (force-output (socket-make-stream sock :output t))
              (setf success t)))
      (unless success (error 'slime-network-error))))
  (values))

;; TODO 2025-10-13: 
(defun slime-secret ()
  "Finds the secret file in the user's home directory.  Returns NIL if the file
doesn't exist; otherwise, returns the first line of the file."
  (let ((secret-file (merge-pathnames (user-homedir-pathname) #p".slime-secret")))
    (with-open-file (input secret-file :if-does-not-exist nil)
      (when input (read-line input nil "")))))

(defun socket-keep-alive (socket)
  "Configures TCP keep alive packets for SOCKET.  The socket connection will be
considered dead if keep alive packets are lost."
  (declare (ignorable socket))
  (setf (sockopt-keep-alive socket) t)
  #+linux
  (setf (sockopt-tcp-keepcnt socket) 1
        (sockopt-tcp-keepidle socket) 30
        (sockopt-tcp-keepintvl socket) 30))

(defun slime-net-connect (host-name port)
  "Establishes a connection to the Swank server listening on PORT of HOST-NAME.
Returns a SWANK-CONNECTION when the connection attempt is successful.
Otherwise, returns NIL.  May signal SLIME-NETWORK-ERROR if the user has a Slime
secret file and there are network problems sending its contents to the remote
Swank server."
  (let ((sock (make-instance 'tcp-socket)))
    (handler-case (socket-connect sock (host-ent-address (get-host-by-name host-name)) port)
      (socket-error (c)
        (signal c)
        (return-from slime-net-connect nil)))
    (socket-keep-alive sock)
    (let ((connection
            (make-instance 'swank-connection :host host-name :port port :socket sock))
          (secret (slime-secret)))
      (when secret (slime-send secret connection))
      connection)))

(defun send-to-emacs (event)
  "Sends EVENT to Emacs."
  (symbol-call :swank 'send 
    (symbol-call :swank 'mconn.control-thread 
      (symbol-call :swank 'default-connection))
    event))

(defun slime-dispatch-event (event connection)
  "Handles EVENT for a Swank CONNECTION.  Signals SLIME-NETWORK-ERROR if there
are communications problems."
  (destructuring-case event
    ((:emacs-rex form package-name thread continuation)
     (let ((id nil))
       (with-mutex ((connection-lock connection))
         (setf id (incf (continuation-counter connection)))
         (push (list id continuation form package-name thread) (rex-continuations connection))
         (when (eq (state connection) :dead) (error 'slime-network-error)))
       (let ((name (format nil "swank sender for ~A/~D" (host connection) (port connection))))
         (make-thread (lambda ()
                        ;; Catch network errors so the Swank sender thread exits gracefully if
                        ;; there are communications problems with the remote Lisp.
                        (handler-case
                            (slime-send `(:emacs-rex ,form ,package-name ,thread ,id) connection)
                          (slime-network-error ())))
                      :name name))))
    ((:return value id)
     (let ((send-to-emacs t))
       (with-mutex ((connection-lock connection))
         (let ((rec (assoc id (rex-continuations connection))))
           (when rec
             (setf send-to-emacs nil)
             (setf (rex-continuations connection) (remove rec (rex-continuations connection)))
             (funcall (second rec) value))))
       ;; The value returned is not for us.  Forward it to Slime.
       (when send-to-emacs
         (force-output)
         (send-to-emacs `(:return ,*current-thread* ,value ,id)))))
    ;; When a remote computation signals a condition and control ends up in the debugger, Swank
    ;; sends these events back to pop up a Slime breakpoint window.  Forward the events to Slime.
    ;; Modify the thread ID of each event to uniquely identify which remote Lisp generated it.
    ((:debug-activate thread level &optional select)
     (incf thread (idx connection))
     (send-to-emacs `(:debug-activate ,thread ,level ,select)))
    ((:debug thread level condition restarts frames continuations)
     (incf thread (idx connection))
     (send-to-emacs `(:debug ,thread ,level ,condition ,restarts ,frames ,continuations)))
    ((:debug-return thread level stepping)
     (incf thread (idx connection))
     (send-to-emacs `(:debug-return ,thread ,level ,stepping)))

    ((:emacs-interrupt thread)
     (slime-send `(:emacs-interrupt ,thread) connection))
    ((:channel-send id msg)
     (print (list :channel-send id msg)))
    ((:emacs-channel-send id msg)
     (slime-send `(:emacs-channel-send ,id ,msg) connection))
    ((:read-from-minibuffer thread tag prompt initial-value)
     (print (list :read-from-minibuffer thread tag prompt initial-value)))
    ((:y-or-n-p thread tag question)
     (print (list :y-or-n-p thread tag question)))
    ((:emacs-return-string thread tag string)
     (slime-send `(:emacs-return-string ,thread ,tag ,string) connection))
    ;; Ignore remote Lisp feature changes.
    ((:new-features features)
     (declare (ignore features)))
    ;; Ignore remote Lisp indentation updates.
    ((:indentation-update info)
     (declare (ignore info)))
    ((:eval-no-wait form)
     (print (list :eval-no-wait form)))
    ((:eval thread tag form-string)
     (print (list :eval thread tag form-string)))
    ((:ed-rpc-no-wait function-name &rest args)
     (print (list :ed-rpc-no-wait function-name '&rest args)))
    ((:ed-rpc thread tag function-name &rest args)
     (print (list :ed-rpc thread tag function-name '&rest args)))
    ((:emacs-return thread tag value)
     (slime-send `(:emacs-return ,thread ,tag ,value) connection))
    ((:ed what)
     (print (list :ed what)))
    ((:inspect what wait-thread wait-tag)
     (print (list :inspect what wait-thread wait-tag)))
    ((:background-message message)
     (print (list :background-message message)))
    ((:debug-condition thread message)
     (assert thread)
     (print (list :debug-condition thread message)))
    ((:ping thread tag)
     (slime-send `(:emacs-pong ,thread ,tag) connection))
    ((:reader-error packet condition)
     (print (list :reader-error packet condition))
     (error "Invalid protocol message"))
    ((:invalid-rpc id message)
     (setf (rex-continuations connection) (remove id (rex-continuations connection) :key #'car))
     (error "Invalid rpc: ~S" message))
    ((:emacs-skipped-packet packet)
     (print (list :emacs-skipped-packet packet)))
    ((t &rest args)
     (error "Unknown event received: ~S" args))))

(defun slime-net-read (connection)
  "Reads a Swank message from a network CONNECTION to a Swank server.  Returns
the Swank event or NIL, if there was a problem reading data."
  (flet ((safe-read-sequence (buffer stream)
           ;; We use IGNORE-ERRORS here to catch SB-INT:CLOSED-STREAM-ERROR on SBCL and any other
           ;; system-dependent network or stream errors.
           (let ((result (ignore-errors (read-sequence buffer stream))))
             (unless result (return-from slime-net-read))
             result)))
    (let ((stream (socket-make-stream (socket connection) :input t))
          (length-buffer (make-octets 6)))
      (if (/= (safe-read-sequence length-buffer stream) 6)
          nil
          (let* ((length-string (sb-ext:octets-to-string length-buffer))
                 (length (parse-integer length-string :radix 16))
                 (message-buffer (make-octets length)))
            (if (/= (safe-read-sequence message-buffer stream) length)
                nil
                (let ((message (sb-ext:octets-to-string message-buffer)))
                  (with-standard-io-syntax
                    (let ((*package* *io-package*))
                      (read-from-string message))))))))))

(defmacro slime-rex ((&rest saved-vars) (sexp connection) &body continuations)
  "(slime-rex (VAR ...) (SEXP CONNECTION) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each VAR is
either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the PRIN1-ed version is sent over CONNECTION to a remote
Lisp.

CLAUSES is a list of patterns with same syntax as `destructuring-case'.  The
result of the evaluation of SEXP is dispatched on CLAUSES.  The result is either
a sexp of the form (:ok VALUE) or (:abort CONDITION).  CLAUSES is executed
asynchronously.

Signals SLIME-NETWORK-ERROR when there are network problems sending SEXP."
  (let ((result (gensym)))
    `(let ,(loop for var in saved-vars
                 collect (etypecase var
                           (symbol (list var var))
                           (cons var)))
       (slime-dispatch-event (list :emacs-rex
                                   ,sexp
                                   "COMMON-LISP-USER"
                                   t
                                   (lambda (,result)
                                     (destructuring-case ,result ,@continuations)))
                             ,connection))))

(defun slime-eval-async (sexp connection &optional continuation)
  "Sends SEXP over CONNECTION to a Swank server for evaluation, then immediately
returns.  Some time later, after the evaluation is finished, CONTINUATION is
called with the result as argument.  Signals SLIME-NETWORK-ERROR when there are
network problems sending SEXP."
  (slime-rex (continuation)
      (sexp connection)
    ((:ok result)
     (when continuation
       (funcall continuation result)))
    ((:abort condition)
     (when continuation
       (funcall continuation (cons +abort+ condition)))))
  (values))

(defun slime-eval (sexp connection)
  "Sends SEXP over CONNECTION to a Swank server for evaluation and waits for the
result.  When the result is received, it is returned.  Signals
SLIME-NETWORK-ERROR when there are network problems sending SEXP."
  (let* ((done-lock (make-mutex :name "slime eval"))
         (done (make-waitqueue))
         (result-available nil)
         (result nil))
    ;; See the Bordeaux Threads documentation for a description of the locking pattern used here.
    (slime-eval-async sexp
                      connection
                      (lambda (x)
                        (with-mutex (done-lock)
                          (setf result x
                                result-available t)
                          (condition-notify done))))
    (with-mutex (done-lock)
      ;; Do not call CONDITION-WAIT if our result is already available, since we would wait forever
      ;; on the DONE condition variable, which has already been notified.  Also, CONDITION-WAIT can
      ;; return spuriously before DONE has been notified, so wait again if our result is not yet
      ;; available.
      (loop until result-available
            do (condition-wait done done-lock)))
    (when (and (consp result) (eq (car result) +abort+))
      (error "Evaluation aborted on ~s." (cdr result)))
    result))

(defun slime-pending-evals-p (connection)
  "Returns T if there are outstanding evaluations pending on CONNECTION;
otherwise, returns NIL."
  (not (null (rex-continuations connection))))

(defun slime-migrate-evals (old-connection new-connection)
  "Evaluates on NEW-CONNECTION all the work pending on a closed OLD-CONNECTION.
Signals SLIME-NETWORK-ERROR when there are network problems."
  (dolist (rec (rex-continuations old-connection))
    (destructuring-bind (id continuation form package-name thread)
        rec
      (declare (ignore id))
      (slime-dispatch-event `(:emacs-rex ,form ,package-name ,thread ,continuation)
                            new-connection)))
  (setf (rex-continuations old-connection) '()))

(defun slime-dispatch-events (connection connection-closed-hook)
  "Reads and dispatches incoming events for a CONNECTION to a Swank server.  If
provided, function CONNECTION-CLOSED-HOOK is called when CONNECTION is closed."
  (flet ((close-connection ()
           (with-mutex ((connection-lock connection))
             (socket-close (socket connection))
             (setf (state connection) :dead))
           (remove-open-connection connection)
           (when connection-closed-hook (funcall connection-closed-hook))))
    (loop (let ((event (slime-net-read connection)))
            (unless event
              (close-connection)
              (return-from slime-dispatch-events))
            ;; TODO(brown): Verify that this call to SLIME-DISPATCH-EVENTS will never signal
            ;; SLIME-NETWORK-ERROR.
            (slime-dispatch-event event connection))
          (let ((state nil))
            (with-mutex ((connection-lock connection))
              (setf state (state connection)))
            (ecase state
              (:alive)
              (:closing
               (close-connection)
               (return-from slime-dispatch-events))
              (:dead
               (return-from slime-dispatch-events)))))))

(defun slime-connect (host-name port &optional connection-closed-hook)
  "Connects to the Swank server running on HOST-NAME that is listening on PORT.
Returns a SWANK-CONNECTION if the connection attempt is successful.  Otherwise,
returns NIL.  May signal SLIME-NETWORK-ERROR if the user has a Slime secret file
and there are network problems sending its contents to the remote Swank server.
If provided, function CONNECTION-CLOSED-HOOK is called when the connection is
closed."
  (let ((connection (slime-net-connect host-name port)))
    (when connection
      (add-open-connection connection)
      ;; Create a thread to handle incoming events from the remote Lisp.
      (let ((name (format nil "swank dispatcher for ~A/~D" host-name port)))
        (make-thread (lambda ()
                       (slime-dispatch-events connection connection-closed-hook))
                     :name name)))
    connection))

(defun slime-connect-file (path &optional connection-closed-hook)
  (with-open-file (f path)
    (slime-connect "localhost" (read f) connection-closed-hook)))

(defun slime-close (connection)
  "Closes CONNECTION to a Swank server."
  (with-mutex ((connection-lock connection))
    (setf (state connection) :closing))
  (slime-eval-async nil connection)
  (values))

(defmethod disconnect ((self swank-connection) &key)
  (slime-close self))

(defmacro with-slime-connection ((variable host-name port &optional connection-closed-hook)
                                 &body body)
  "Wraps BODY in a LET form where VARIABLE is bound to the value returned by
(SLIME-CONNECT HOST-NAME PORT CONNECTION-CLOSED-HOOK).  Arranges for the Swank
connection to be closed when control exits BODY."
  `(let ((,variable (slime-connect ,host-name ,port ,connection-closed-hook)))
     (unless ,variable (error 'slime-network-error))
     (unwind-protect
          (progn ,@body)
       (slime-close ,variable))))

;;; Remote Execution (RDP)

;;;; Messages

;; (:emacs-rex form package thread cont)

;; (:return return-expression cont)

;; :write-string

;; :new-package

;; :debug

;; :debug-activate

;; :indentation-update
