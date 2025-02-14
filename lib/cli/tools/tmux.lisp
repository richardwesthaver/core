;;; cli/tmux.lisp --- Tmux Tools

;; Control Tmux from Lisp

;;; Commentary:

;; ref: https://github.com/tmux/tmux/wiki/Getting-Started#getting-started

;; By default tmux tries to open a TTY and errors when it can't, so normally
;; you'd want to use SPAWN-TMUX to allocate a terminal first.

;; There is however a control-mode available which provides a text-based
;; channel without needing a TTY.

;; To use this mode call RUN-TMUX with the "-C" arg.

;; ref: https://github.com/tmux/tmux/wiki/Control-Mode#control-mode

;;; Code:
(in-package :cli/tools/tmux)

(deferror tmux-error (simple-error) () (:auto t))

(defparameter *tmux-user-config-path* (merge-pathnames ".tmux.conf" (user-homedir-pathname)))
(defparameter *tmux-system-config-path* (merge-pathnames "tmux.conf" "/etc/"))

(defparameter *tmux* (find-exe "tmux"))

(defparameter *default-tmux-tmpdir* (pathname (format nil "/tmp/tmux-~A/" (sb-posix:getuid))))
(defparameter *default-tmux-socket* (merge-pathnames "default" *default-tmux-tmpdir*))

;;; Utils
(define-cli-tool :tmux (&rest args)
  (let ((proc (sb-ext:run-program *tmux* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (tmux-error "tmux command failed: ~A ~A" args))))

(defun spawn-tmux (&rest args)
  (run-term (append (list "-e" "tmux") args)))

;;; Session > Window > Pane
(defstruct tmux-session
  (id 0 :type fixnum)
  name
  (windows nil :type list))

(defstruct tmux-window
  (id 0 :type fixnum)
  name
  (panes nil :type list)
  layout)

(defstruct tmux-pane
  (id 0 :type fixnum)
  name)

;;; Controller
(defstruct tmux-controller
  (input nil :type (or null sb-sys:fd-stream))
  (output nil :type (or null sb-sys:fd-stream))
  (silent nil :type boolean))

(defun run-tmux-controller (&rest args)
  (sb-ext:run-program *tmux* (or args nil) :output :stream :input :stream))

(defun init-tmux-controller (ctrl &rest args)
  (let ((proc (funcall
               #'run-tmux-controller
               (if (tmux-controller-silent ctrl) "-CC" "-C")
               args)))
    (setf (tmux-controller-output ctrl) (sb-ext:process-output proc)
          (tmux-controller-input ctrl) (sb-ext:process-input proc))
    ctrl))

(defun write-tmux-line (ctrl string)
  (write-line string (tmux-controller-input ctrl)))

(defun read-tmux-line (ctrl)
  (read-line (tmux-controller-output ctrl)))

(defstruct tmux-command name flags args)

(defun parse-tmux-command (str)
  "Parse a single TMUX-COMMAND from a string."
  (let ((words (split-sequence #\space str)))
    ;; TODO 2024-08-06: parse for real
    (make-tmux-command :name (car words) :args (cdr words))))

(defconfig tmux-config ()
  ((commands :initform nil)
   (server-options :type hash-table)
   (session-options :type hash-table)
   (window-options :type hash-table)
   (keys :type hash-table))
  (:documentation "A CONFIG object containing the parsed content of a tmux configuration file."))

(defmethod make-config ((obj (eql :tmux)) &key commands server session window keys)
  (let ((config (make-instance 'tmux-config)))
    (when commands (setf (slot-value config 'commands) commands))
    (when server (setf (slot-value config 'server-options) server))
    (when session (setf (slot-value config 'session-options) session))
    (when window (setf (slot-value config 'window-options) window))
    (when keys (setf (slot-value config 'keys) keys))
    config))

(defmethod find-config ((obj (eql :tmux)) &key system user)
  "Find a tmux configuration and load it.

When SYSTEM is non-nil, skip check for user config.

When USER is non-nil it should be the name of a user whose config will be loaded
from /home/USER/.tmux.conf."
  (let ((path (cond
                (system (probe-file *tmux-system-config-path*))
                (user (probe-file (format nil "/home/~A/.tmux.conf" user)))
                (t (or (probe-file *tmux-user-config-path*) (probe-file *tmux-system-config-path*)))))
        (obj (make-config :tmux :commands nil)))
    (with-open-file (file path)
      (with-output-to-string (str)
        (loop for l = (read-line file nil nil)
              while l 
              unless (or (zerop (length l)) (equal (char l 0) #\#))
              do (push (parse-tmux-command l) (slot-value obj 'commands)))))
    obj))

;; (describe (find-config :tmux))

;;; Format Strings
(defun format-tmux-string (dst fmt &rest args)
  (apply #'format dst fmt (mapcar (lambda (a) (format nil "#{~A}" a)) args)))

(defvar *tmux-var-table* (make-hash-table))

(defmacro tmux-format (dst fmt &rest args)
  "Format a tmux string, replacing symbols in ARGS that match a member of
*TMUX-VARIABLES* with their corresponding lower-case name."
  `(format-tmux-string ,dst ,fmt
                       ,@(mapcar (lambda (a)
                                   (gethash (symbolicate a) *tmux-var-table* a))
                                 args)))

(declaim ((vector symbol) *tmux-variables*))
(defvar *tmux-variables*
  #(active-window-index ;; Index of active window in session
    alternate-on ;; 1 if pane is in alternate screen
    alternate-saved-x ;; Saved cursor X in alternate screen
    alternate-saved-y ;; Saved cursor Y in alternate screen
    buffer-created ;; Time buffer created
    buffer-name ;; Name of buffer
    buffer-sample ;; Sample of start of buffer
    buffer-size ;; Size of the specified buffer in bytes
    client-activity ;; Time client last had activity
    client-cell-height ;; Height of each client cell in pixels
    client-cell-width ;; Width of each client cell in pixels
    client-control-mode ;; 1 if client is in control mode
    client-created ;; Time client created
    client-discarded ;; Bytes discarded when client behind
    client-flags ;; List of client flags
    client-height ;; Height of client
    client-key-table ;; Current key table
    client-last-session ;; Name of the client's last session
    client-name ;; Name of client
    client-pid ;; PID of client process
    client-prefix ;; 1 if prefix key has been pressed
    client-readonly ;; 1 if client is read-only
    client-session ;; Name of the client's session
    client-termfeatures ;; Terminal features of client, if any
    client-termname ;; Terminal name of client
    client-termtype ;; Terminal type of client, if available
    client-tty ;; Pseudo terminal of client
    client-uid ;; UID of client process
    client-user ;; User of client process
    client-utf8 ;; 1 if client supports UTF-8
    client-width ;; Width of client
    client-written ;; Bytes written to client
    command ;; Name of command in use, if any
    command-list-alias ;; Command alias if listing commands
    command-list-name ;; Command name if listing commands
    command-list-usage ;; Command usage if listing commands
    config-files ;; List of configuration files loaded
    copy-cursor-line ;; Line the cursor is on in copy mode
    copy-cursor-word ;; Word under cursor in copy mode
    copy-cursor-x ;; Cursor X position in copy mode
    copy-cursor-y ;; Cursor Y position in copy mode
    current-file ;; Current configuration file
    cursor-character ;; Character at cursor in pane
    cursor-flag ;; Pane cursor flag
    cursor-x ;; Cursor X position in pane
    cursor-y ;; Cursor Y position in pane
    history-bytes ;; Number of bytes in window history
    history-limit ;; Maximum window history lines
    history-size ;; Size of history in lines
    hook ;; Name of running hook, if any
    hook-client ;; Name of client where hook was run, if any
    hook-pane ;; ID of pane where hook was run, if any
    hook-session ;; ID of session where hook was run, if any
    hook-session-name ;; Name of session where hook was run, if any
    hook-window ;; ID of window where hook was run, if any
    hook-window-name ;; Name of window where hook was run, if any
    host ;; H	Hostname of local host
    host-short ;; h	Hostname of local host (no domain name)
    insert-flag ;; Pane insert flag
    keypad-cursor-flag ;; Pane keypad cursor flag
    keypad-flag ;; Pane keypad flag
    last-window-index ;; Index of last window in session
    line ;; Line number in the list
    mouse-all-flag ;; Pane mouse all flag
    mouse-any-flag ;; Pane mouse any flag
    mouse-button-flag ;; Pane mouse button flag
    mouse-hyperlink ;; Hyperlink under mouse, if any
    mouse-line ;; Line under mouse, if any
    mouse-sgr-flag ;; Pane mouse SGR flag
    mouse-standard-flag ;; Pane mouse standard flag
    mouse-status-line ;; Status line on which mouse event took place
    mouse-status-range ;; Range type or argument of mouse event on status line
    mouse-utf8-flag ;; Pane mouse UTF-8 flag
    mouse-word ;; Word under mouse, if any
    mouse-x ;; Mouse X position, if any
    mouse-y ;; Mouse Y position, if any
    next-session-id ;; Unique session ID for next new session
    origin-flag ;; Pane origin flag
    pane-active ;; 1 if active pane
    pane-at-bottom ;; 1 if pane is at the bottom of window
    pane-at-left ;; 1 if pane is at the left of window
    pane-at-right ;; 1 if pane is at the right of window
    pane-at-top ;; 1 if pane is at the top of window
    pane-bg ;; Pane background colour
    pane-bottom ;; Bottom of pane
    pane-current-command ;; Current command if available
    pane-current-path ;; Current path if available
    pane-dead ;; 1 if pane is dead
    pane-dead-signal ;; Exit signal of process in dead pane
    pane-dead-status ;; Exit status of process in dead pane
    pane-dead-time ;; Exit time of process in dead pane
    pane-fg ;; Pane foreground colour
    pane-format ;; 1 if format is for a pane
    pane-height ;; Height of pane
    pane-id ;; D	Unique pane ID
    pane-in-mode ;; 1 if pane is in a mode
    pane-index ;; P	Index of pane
    pane-input-off ;; 1 if input to pane is disabled
    pane-last ;; 1 if last pane
    pane-left ;; Left of pane
    pane-marked ;; 1 if this is the marked pane
    pane-marked-set ;; 1 if a marked pane is set
    pane-mode ;; Name of pane mode, if any
    pane-path ;; Path of pane (can be set by application)
    pane-pid ;; PID of first process in pane
    pane-pipe ;; 1 if pane is being piped
    pane-right ;; Right of pane
    pane-search-string ;; Last search string in copy mode
    pane-start-command ;; Command pane started with
    pane-start-path ;; Path pane started with
    pane-synchronized ;; 1 if pane is synchronized
    pane-tabs ;; Pane tab positions
    pane-title ;; T	Title of pane (can be set by application)
    pane-top ;; Top of pane
    pane-tty ;; Pseudo terminal of pane
    pane-unseen-changes ;; 1 if there were changes in pane while in mode
    pane-width ;; Width of pane
    pid ;; Server PID
    rectangle-toggle ;; 1 if rectangle selection is activated
    scroll-position ;; Scroll position in copy mode
    scroll-region-lower ;; Bottom of scroll region in pane
    scroll-region-upper ;; Top of scroll region in pane
    search-match ;; Search match if any
    search-present ;; 1 if search started in copy mode
    selection-active ;; 1 if selection started and changes with the cursor in copy mode
    selection-end-x ;; X position of the end of the selection
    selection-end-y ;; Y position of the end of the selection
    selection-present ;; 1 if selection started in copy mode
    selection-start-x ;; X position of the start of the selection
    selection-start-y ;; Y position of the start of the selection
    server-sessions ;; Number of sessions
    session-activity ;; Time of session last activity
    session-alerts ;; List of window indexes with alerts
    session-attached ;; Number of clients session is attached to
    session-attached-list ;; List of clients session is attached to
    session-created ;; Time session created
    session-format ;; 1 if format is for a session
    session-group ;; Name of session group
    session-group-attached ;; Number of clients sessions in group are attached to
    session-group-attached-list ;; List of clients sessions in group are attached to
    session-group-list ;; List of sessions in group
    session-group-many-attached ;; 1 if multiple clients attached to sessions in group
    session-group-size ;; Size of session group
    session-grouped ;; 1 if session in a group
    session-id ;; Unique session ID
    session-last-attached ;; Time session last attached
    session-many-attached ;; 1 if multiple clients attached
    session-marked ;; 1 if this session contains the marked pane
    session-name ;; S	Name of session
    session-path ;; Working directory of session
    session-stack ;; Window indexes in most recent order
    session-windows ;; Number of windows in session
    socket-path ;; Server socket path
    start-time ;; Server start time
    uid ;; Server UID
    user ;; Server user
    version ;; Server version
    window-active ;; 1 if window active
    window-active-clients ;; Number of clients viewing this window
    window-active-clients-list ;; List of clients viewing this window
    window-active-sessions ;; Number of sessions on which this window is active
    window-active-sessions-list ;; List of sessions on which this window is active
    window-activity ;; Time of window last activity
    window-activity-flag ;; 1 if window has activity
    window-bell-flag ;; 1 if window has bell
    window-bigger ;; 1 if window is larger than client
    window-cell-height ;; Height of each cell in pixels
    window-cell-width ;; Width of each cell in pixels
    window-end-flag ;; 1 if window has the highest index
    window-flags ;; F	Window flags with # escaped as ##
    window-format ;; 1 if format is for a window
    window-height ;; Height of window
    window-id ;; Unique window ID
    window-index ;; I	Index of window
    window-last-flag ;; 1 if window is the last used
    window-layout ;; Window layout description, ignoring zoomed window panes
    window-linked ;; 1 if window is linked across sessions
    window-linked-sessions ;; Number of sessions this window is linked to
    window-linked-sessions-list ;; List of sessions this window is linked to
    window-marked-flag ;; 1 if window contains the marked pane
    window-name ;; W	Name of window
    window-offset-x ;; X offset into window if larger than client
    window-offset-y ;; Y offset into window if larger than client
    window-panes ;; Number of panes in window
    window-raw-flags ;; Window flags with nothing escaped
    window-silence-flag ;; 1 if window has silence alert
    window-stack-index ;; Index in session most recent stack
    window-start-flag ;; 1 if window has the lowest index
    window-visible-layout ;; Window layout description, respecting zoomed window panes
    window-width ;; Width of window
    window-zoomed-flag ;; 1 if window is zoomed
    wrap-flag ;; Pane wrap flag
    ;; display-menu vars
    popup-centre-x	Centered in the client
    popup-centre-y ;; entered in the client
    popup-height ;; eight of menu or popup
    popup-mouse-bottom ;; ottom of at the mouse
    popup-mouse-centre-x ;; orizontal centre at the mouse
    popup-mouse-centre-y ;; ertical centre at the mouse
    popup-mouse-top ;; op at the mouse
    popup-mouse-x ;; ouse X position
    popup-mouse-y ;; ouse Y position
    popup-pane-bottom ;; ottom of the pane
    popup-pane-left ;; eft of the pane
    popup-pane-right ;; ight of the pane
    popup-pane-top ;; op of the pane
    popup-status-line-y ;; bove or below the status line
    popup-width ;; idth of menu or popup
    popup-window-status-line-x ;; t the window position in status line
    popup-window-status-line-y ;; t the status line showing the window
    ))

(defvar *tmux-variable-names*
  (coerce 
   (loop for v across *tmux-variables*
         collect (string-downcase (substitute #\_ #\- (symbol-name v))))
   '(vector string)))
