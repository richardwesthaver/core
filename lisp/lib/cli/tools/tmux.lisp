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

(deferror tmux-error (simple-error error) ())

(defun tmux-error (fmt &rest args)
  (error 'tmux-error :format-arguments args :format-control fmt))

(defparameter *tmux-user-config-path* (merge-pathnames ".tmux.conf" (user-homedir-pathname)))
(defparameter *tmux-system-config-path* (merge-pathnames "tmux.conf" "/etc/"))

(defparameter *tmux* (find-exe "tmux"))

(defparameter *default-tmux-tmpdir* (pathname (format nil "/tmp/tmux-~A/" (sb-posix:getuid))))
(defparameter *default-tmux-socket* (merge-pathnames "default" *default-tmux-tmpdir*))

(defstruct tmux-session)
(defstruct tmux-window)
(defstruct tmux-pane)

(defun run-tmux (&rest args)
  (let ((proc (sb-ext:run-program *tmux* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (tmux-error "tmux command failed: ~A ~A" args))))

(defun spawn-tmux (&rest args)
  (run-terminal (append (list "-e" "tmux") args)))

;;; Format Strings
(defun format-tmux-string (dst fmt &rest args)
  (apply #'format dst fmt (mapcar (lambda (a) (format nil "#{~A}" a)) args)))

(defvar *tmux-variables*
  '(active_window_index ;; Index of active window in session
    alternate_on ;; 1 if pane is in alternate screen
    alternate_saved_x ;; Saved cursor X in alternate screen
    alternate_saved_y ;; Saved cursor Y in alternate screen
    buffer_created ;; Time buffer created
    buffer_name ;; Name of buffer
    buffer_sample ;; Sample of start of buffer
    buffer_size ;; Size of the specified buffer in bytes
    client_activity ;; Time client last had activity
    client_cell_height ;; Height of each client cell in pixels
    client_cell_width ;; Width of each client cell in pixels
    client_control_mode ;; 1 if client is in control mode
    client_created ;; Time client created
    client_discarded ;; Bytes discarded when client behind
    client_flags ;; List of client flags
    client_height ;; Height of client
    client_key_table ;; Current key table
    client_last_session ;; Name of the client's last session
    client_name ;; Name of client
    client_pid ;; PID of client process
    client_prefix ;; 1 if prefix key has been pressed
    client_readonly ;; 1 if client is read-only
    client_session ;; Name of the client's session
    client_termfeatures ;; Terminal features of client, if any
    client_termname ;; Terminal name of client
    client_termtype ;; Terminal type of client, if available
    client_tty ;; Pseudo terminal of client
    client_uid ;; UID of client process
    client_user ;; User of client process
    client_utf8 ;; 1 if client supports UTF-8
    client_width ;; Width of client
    client_written ;; Bytes written to client
    command ;; Name of command in use, if any
    command_list_alias ;; Command alias if listing commands
    command_list_name ;; Command name if listing commands
    command_list_usage ;; Command usage if listing commands
    config_files ;; List of configuration files loaded
    copy_cursor_line ;; Line the cursor is on in copy mode
    copy_cursor_word ;; Word under cursor in copy mode
    copy_cursor_x ;; Cursor X position in copy mode
    copy_cursor_y ;; Cursor Y position in copy mode
    current_file ;; Current configuration file
    cursor_character ;; Character at cursor in pane
    cursor_flag ;; Pane cursor flag
    cursor_x ;; Cursor X position in pane
    cursor_y ;; Cursor Y position in pane
    history_bytes ;; Number of bytes in window history
    history_limit ;; Maximum window history lines
    history_size ;; Size of history in lines
    hook ;; Name of running hook, if any
    hook_client ;; Name of client where hook was run, if any
    hook_pane ;; ID of pane where hook was run, if any
    hook_session ;; ID of session where hook was run, if any
    hook_session_name ;; Name of session where hook was run, if any
    hook_window ;; ID of window where hook was run, if any
    hook_window_name ;; Name of window where hook was run, if any
    host ;; H	Hostname of local host
    host_short ;; h	Hostname of local host (no domain name)
    insert_flag ;; Pane insert flag
    keypad_cursor_flag ;; Pane keypad cursor flag
    keypad_flag ;; Pane keypad flag
    last_window_index ;; Index of last window in session
    line ;; Line number in the list
    mouse_all_flag ;; Pane mouse all flag
    mouse_any_flag ;; Pane mouse any flag
    mouse_button_flag ;; Pane mouse button flag
    mouse_hyperlink ;; Hyperlink under mouse, if any
    mouse_line ;; Line under mouse, if any
    mouse_sgr_flag ;; Pane mouse SGR flag
    mouse_standard_flag ;; Pane mouse standard flag
    mouse_status_line ;; Status line on which mouse event took place
    mouse_status_range ;; Range type or argument of mouse event on status line
    mouse_utf8_flag ;; Pane mouse UTF-8 flag
    mouse_word ;; Word under mouse, if any
    mouse_x ;; Mouse X position, if any
    mouse_y ;; Mouse Y position, if any
    next_session_id ;; Unique session ID for next new session
    origin_flag ;; Pane origin flag
    pane_active ;; 1 if active pane
    pane_at_bottom ;; 1 if pane is at the bottom of window
    pane_at_left ;; 1 if pane is at the left of window
    pane_at_right ;; 1 if pane is at the right of window
    pane_at_top ;; 1 if pane is at the top of window
    pane_bg ;; Pane background colour
    pane_bottom ;; Bottom of pane
    pane_current_command ;; Current command if available
    pane_current_path ;; Current path if available
    pane_dead ;; 1 if pane is dead
    pane_dead_signal ;; Exit signal of process in dead pane
    pane_dead_status ;; Exit status of process in dead pane
    pane_dead_time ;; Exit time of process in dead pane
    pane_fg ;; Pane foreground colour
    pane_format ;; 1 if format is for a pane
    pane_height ;; Height of pane
    pane_id ;; D	Unique pane ID
    pane_in_mode ;; 1 if pane is in a mode
    pane_index ;; P	Index of pane
    pane_input_off ;; 1 if input to pane is disabled
    pane_last ;; 1 if last pane
    pane_left ;; Left of pane
    pane_marked ;; 1 if this is the marked pane
    pane_marked_set ;; 1 if a marked pane is set
    pane_mode ;; Name of pane mode, if any
    pane_path ;; Path of pane (can be set by application)
    pane_pid ;; PID of first process in pane
    pane_pipe ;; 1 if pane is being piped
    pane_right ;; Right of pane
    pane_search_string ;; Last search string in copy mode
    pane_start_command ;; Command pane started with
    pane_start_path ;; Path pane started with
    pane_synchronized ;; 1 if pane is synchronized
    pane_tabs ;; Pane tab positions
    pane_title ;; T	Title of pane (can be set by application)
    pane_top ;; Top of pane
    pane_tty ;; Pseudo terminal of pane
    pane_unseen_changes ;; 1 if there were changes in pane while in mode
    pane_width ;; Width of pane
    pid ;; Server PID
    rectangle_toggle ;; 1 if rectangle selection is activated
    scroll_position ;; Scroll position in copy mode
    scroll_region_lower ;; Bottom of scroll region in pane
    scroll_region_upper ;; Top of scroll region in pane
    search_match ;; Search match if any
    search_present ;; 1 if search started in copy mode
    selection_active ;; 1 if selection started and changes with the cursor in copy mode
    selection_end_x ;; X position of the end of the selection
    selection_end_y ;; Y position of the end of the selection
    selection_present ;; 1 if selection started in copy mode
    selection_start_x ;; X position of the start of the selection
    selection_start_y ;; Y position of the start of the selection
    server_sessions ;; Number of sessions
    session_activity ;; Time of session last activity
    session_alerts ;; List of window indexes with alerts
    session_attached ;; Number of clients session is attached to
    session_attached_list ;; List of clients session is attached to
    session_created ;; Time session created
    session_format ;; 1 if format is for a session
    session_group ;; Name of session group
    session_group_attached ;; Number of clients sessions in group are attached to
    session_group_attached_list ;; List of clients sessions in group are attached to
    session_group_list ;; List of sessions in group
    session_group_many_attached ;; 1 if multiple clients attached to sessions in group
    session_group_size ;; Size of session group
    session_grouped ;; 1 if session in a group
    session_id ;; Unique session ID
    session_last_attached ;; Time session last attached
    session_many_attached ;; 1 if multiple clients attached
    session_marked ;; 1 if this session contains the marked pane
    session_name ;; S	Name of session
    session_path ;; Working directory of session
    session_stack ;; Window indexes in most recent order
    session_windows ;; Number of windows in session
    socket_path ;; Server socket path
    start_time ;; Server start time
    uid ;; Server UID
    user ;; Server user
    version ;; Server version
    window_active ;; 1 if window active
    window_active_clients ;; Number of clients viewing this window
    window_active_clients_list ;; List of clients viewing this window
    window_active_sessions ;; Number of sessions on which this window is active
    window_active_sessions_list ;; List of sessions on which this window is active
    window_activity ;; Time of window last activity
    window_activity_flag ;; 1 if window has activity
    window_bell_flag ;; 1 if window has bell
    window_bigger ;; 1 if window is larger than client
    window_cell_height ;; Height of each cell in pixels
    window_cell_width ;; Width of each cell in pixels
    window_end_flag ;; 1 if window has the highest index
    window_flags ;; F	Window flags with # escaped as ##
    window_format ;; 1 if format is for a window
    window_height ;; Height of window
    window_id ;; Unique window ID
    window_index ;; I	Index of window
    window_last_flag ;; 1 if window is the last used
    window_layout ;; Window layout description, ignoring zoomed window panes
    window_linked ;; 1 if window is linked across sessions
    window_linked_sessions ;; Number of sessions this window is linked to
    window_linked_sessions_list ;; List of sessions this window is linked to
    window_marked_flag ;; 1 if window contains the marked pane
    window_name ;; W	Name of window
    window_offset_x ;; X offset into window if larger than client
    window_offset_y ;; Y offset into window if larger than client
    window_panes ;; Number of panes in window
    window_raw_flags ;; Window flags with nothing escaped
    window_silence_flag ;; 1 if window has silence alert
    window_stack_index ;; Index in session most recent stack
    window_start_flag ;; 1 if window has the lowest index
    window_visible_layout ;; Window layout description, respecting zoomed window panes
    window_width ;; Width of window
    window_zoomed_flag ;; 1 if window is zoomed
    wrap_flag ;; Pane wrap flag
    ;; display-menu vars
    popup_centre_x	Centered in the client
    popup_centre_y ;; entered in the client
    popup_height ;; eight of menu or popup
    popup_mouse_bottom ;; ottom of at the mouse
    popup_mouse_centre_x ;; orizontal centre at the mouse
    popup_mouse_centre_y ;; ertical centre at the mouse
    popup_mouse_top ;; op at the mouse
    popup_mouse_x ;; ouse X position
    popup_mouse_y ;; ouse Y position
    popup_pane_bottom ;; ottom of the pane
    popup_pane_left ;; eft of the pane
    popup_pane_right ;; ight of the pane
    popup_pane_top ;; op of the pane
    popup_status_line_y ;; bove or below the status line
    popup_width ;; idth of menu or popup
    popup_window_status_line_x ;; t the window position in status line
    popup_window_status_line_y ;; t the status line showing the window
    ))

(defvar *tmux-var-table*
  (let ((tbl (make-hash-table :test 'equal :size (length *tmux-variables*))))
    (dolist (v *tmux-variables* tbl)
      (setf (gethash v tbl) (string-downcase (symbol-name v))))))

(defmacro tmux-format (dst fmt &rest args)
  "Format a tmux string, replacing symbols in ARGS that match a member of
*TMUX-VARIABLES* with their corresponding lower-case name."
  `(format-tmux-string ,dst ,fmt
                       ,@(mapcar (lambda (a)
                                   (gethash (symbolicate a) *tmux-var-table* a))
                                 args)))


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

