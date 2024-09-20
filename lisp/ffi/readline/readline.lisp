;;; readline/readline.lisp --- Readline Alien Routines

;; This implementation is based on Vindarel's cl-readline: https://github.com/vindarel/cl-readline

;;; Code:
(in-package :readline)

(define-alien-enum (rl-completion-type int :test eq)
                   :standard-completion 9
                   :display-and-perform 33
                   :insert-all 42
                   :list-all 63
                   :not-list-cmn-prefix 64)

(define-alien-type rl-history-entry (struct rl-history-entry
                                            (line (* t))
                                            (time (* t))
                                            (data (* t))))
(macrolet ((def-rl-var (name var type)
             `(define-alien-variable (,name ,var) ,type)))
  (def-rl-var "rl_line_buffer" *line-buffer* c-string)
  (def-rl-var "rl_point" *point* int)
  (def-rl-var "rl_end" *end* int)
  (def-rl-var "rl_mark" *mark* int)
  (def-rl-var "rl_done" *point* boolean)
  (def-rl-var "rl_num_chars_to_read" *num-chars-to-read* int)
  (def-rl-var "rl_pending_input" *pending-input* int)
  (def-rl-var "rl_dispatching" *point* boolean)
  (def-rl-var "rl_erase_empty_line" *erase-empty-line* boolean)
  (def-rl-var "rl_prompt" *prompt* c-string)
  (def-rl-var "rl_display_prompt" *display-prompt* c-string)
  (def-rl-var "rl_already_prompted" *already-prompted* boolean)
  (def-rl-var "rl_library_version" *library-version* c-string)
  (def-rl-var "rl_readline_version" *readline-version* int)
  (def-rl-var "rl_gnu_readline_p" *gnu-readline-p* boolean)
  (def-rl-var "rl_terminal_name" *terminal-name* c-string)
  (def-rl-var "rl_readline_name" *readline-name* c-string)
  (def-rl-var "rl_instream" *instream* (* t))
  (def-rl-var "rl_outstream" *outstream* (* t))
  (def-rl-var "rl_prefer_env_winsize" *prefer-env-winsize* boolean)
  (def-rl-var "rl_last_func" *last-func* (* t))
  (def-rl-var "rl_startup_hook" *startup-hook* (* t))
  (def-rl-var "rl_pre_input_hook" *pre-input-hook* (* t))
  (def-rl-var "rl_event_hook" *event-hook* (* t))
  (def-rl-var "rl_getc_function" *getc-function* (* t))
  (def-rl-var "rl_signal_event_hook" *signal-event-hook* (* t))
  (def-rl-var "rl_input-available_hook" *input-available-hook* (* t))
  (def-rl-var "rl_redisplay_function" *redisplay-function* (* t))
  (def-rl-var "rl_prep_term_function" *prep-term-function* (* t))
  (def-rl-var "rl_deprep_term_function" *deprep-term-function* (* t))
  (def-rl-var "rl_executing_keymap" *executing-keymap* (* t))
  (def-rl-var "rl_binding_keymap" *binding-keymap* (* t))
  (def-rl-var "rl_executing_macro" *executing-macro* c-string)
  (def-rl-var "rl_executing_key" *executing-key* char)
  (def-rl-var "rl_executing_keyseq" *executing-keyseq* c-string)
  (def-rl-var "rl_key_sequence_length" *key-sequence-length* int)
  (def-rl-var "rl_readline_state" *readline-state* int)  
  (def-rl-var "rl_explicit_arg" *explicit-arg* boolean)
  (def-rl-var "rl_numeric_arg" *numeric-arg* int)
  (def-rl-var "rl_editing_mode" *editing-mode* int)
  (def-rl-var "rl_catch_sigwinch" *catch-sigwinch* boolean)
  (def-rl-var "rl_change_environment" *change-environment* boolean)
  (def-rl-var "rl_attempted_completion_function" *attempted-completion-function* (* t))
  (def-rl-var "rl_completion_display_matches_hook" *completion-display-matches-hook* (* t))
  (def-rl-var "rl_basic_word_break_characters" *basic-word-break-characters* c-string)
  (def-rl-var "rl_completer_word_break_character" *completer-word-break-characters* c-string)
  (def-rl-var "rl_completion_query_items" *completer-query-items* int)
  (def-rl-var "rl_completion_append_character" *completion-append-character* char)
  (def-rl-var "rl_ignore_completion_duplicates" *ignore-completion-duplicates* boolean)
  (def-rl-var "rl_attempted_completion_over" *attempted-completion-over* boolean)
  (def-rl-var "rl_sort_completion_matches" *sort-completion-matches* boolean)
  (def-rl-var "rl_completion_type" *completion-type* rl-completion-type)
  (def-rl-var "rl_inhibit_completion" *inhibit-completion* boolean)
  (def-rl-var "history_base" *history-base* int)
  (def-rl-var "history_length" *history-length* int))

(defvar *states*
  '(:initializing ; 0x0000001 initializing
    :initialized  ; 0x0000002 initialization done
    :termprepped  ; 0x0000004 terminal is prepped
    :readcmd      ; 0x0000008 reading a command key
    :metanext     ; 0x0000010 reading input after ESC
    :dispatching  ; 0x0000020 dispatching to a command
    :moreinput    ; 0x0000040 reading more input in a command function
    :isearch      ; 0x0000080 doing incremental search
    :nsearch      ; 0x0000100 doing non-incremental search
    :search       ; 0x0000200 doing a history search
    :numericarg   ; 0x0000400 reading numeric argument
    :macroinput   ; 0x0000800 getting input from a macro
    :macrodef     ; 0x0001000 defining keyboard macro
    :overwrite    ; 0x0002000 overwrite mode
    :completing   ; 0x0004000 doing completion
    :sighandler   ; 0x0008000 in readline sighandler
    :undoing      ; 0x0010000 doing an undo
    :inputpending ; 0x0020000 rl_execute_next called
    :ttycsaved    ; 0x0040000 tty special chars saved
    :callback     ; 0x0080000 using the callback interface
    :vimotion     ; 0x0100000 reading vi motion arg
    :multikey     ; 0x0200000 reading multiple-key command
    :vicmdonce    ; 0x0400000 entered vi command mode at least once
    :redisplaying ; 0x0800000 updating terminal display
    :done)        ; 0x1000000 done; accepted line
  "Possible state values for `+readline-state+'.")

(defvar +c-buffer-size+ 256
  "How many bytes to allocate per Lisp string when converting list of
Lisp strings into array of C strings.")

(defun decode-version (version)
  "Transform VERSION into two values representing major and minor numbers of
Readline library version."
  (values (ldb (byte 8 8) version)
          (ldb (byte 8 0) version)))

;; (defun decode-state (state)
;;   "Transform Readline state STATE into list of keywords. See `+states+' for
;; list of components that can appear in result list."
;;   (mapcan (lambda (index keyword)
;;             (when (logbitp index state)
;;               (list keyword)))
;;           (iota (length +states+))
;;           +states+))

(defmacro produce-callback (function return-type &optional func-arg-list)
  "Return pointer to callback that calls FUNCTION. RETURN-TYPE specifies
return type of the function and FUNC-ARG-LIST is list of argument types (it
can be ommited if FUNCTION doesn't take any arguments)."
  (let ((gensymed-list (mapcar (lambda (x) (list (gensym) x))
                               func-arg-list)))
    (std:with-gensyms (temp)
      `(when ,function
         (progn
           (define-alien-callable ,temp ,return-type ,gensymed-list
             (funcall ,function ,@(mapcar #'car gensymed-list)))
           (alien-callable-function ',temp))))))

(defun produce-callback* (function return-type &optional func-arg-list)
  "Variant of PRODUCE-CALLBACK that should hopefully be more portable.
This avoids using a GENSYM as the name of a callback, and is also funcallable."
  (let ((gensymed-list (mapcar (lambda (x) (list (gensym) x))
                               func-arg-list)))
    (std:with-gensyms (temp)
      (when function
        (progn
          (eval `(define-alien-callable ,temp ,return-type ,gensymed-list
                   (funcall ,function ,@(mapcar #'car gensymed-list))))
          (alien-callable-function temp))))))

;;; cl-readline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recent-history-line-satisfies-p (predicate)
  "Check if the most recent history line satisfies given predicate
PREDICATE. Return T if there is no history saved."
  (if (zerop *history-length*)
      t
      (with-alien ((s rl-history-entry))
        (funcall predicate
                 ;; TODO 2024-09-19: does SBCL know how to conver this to a lisp string automatically?
                 (with-alien-slots
                     (line)
                     ;; (alien-funcall "history_get"
                     ;;                :int 
                     ;;                (1- (+ *history-base*
                     ;;                       *history-length*)))
                     s
                   line)))))

(define-alien-routine "readline" (* t) (prompt c-string))
(define-alien-routine "add_history" void (line c-string))

(defun rl (&key
           prompt
           already-prompted
           num-chars
           erase-empty-line
           add-history
           novelty-check)
  "Get a line from user with editing. PROMPT, if supplied, is printed before
reading of input. Non-NIL value of ALREADY-PROMPTED will tell Readline that
the application has printed prompt already. However, PROMPT must be supplied
in this case too, so redisplay functions can update the display properly. If
NUM-CHARS argument is a positive number, Readline will return after
accepting that many characters. If ERASE-EMPTY-LINE is not NIL, `readline'
will completely erase the current line, including any prompt, any time a
newline is typed as the only character on an otherwise-empty line. The
cursor is moved to the beginning of the newly-blank line. Supplying
ADD-HISTORY tells Readline that user's input should be added to
history. However, blank lines don't get into history anyway. NOVELTY-CHECK,
if given, must be a predicate that takes two strings: the actual line and
the most recent history line. Only when the predicate evaluates to non-NIL
value new line will be added to the history. Return value on success is the
actual string and NIL on failure."
  (setf *already-prompted*  already-prompted
        *num-chars-to-read* (or num-chars 0)
        *erase-empty-line*  erase-empty-line)
  (let* ((prompt (if prompt (string prompt) ""))
         (ptr (readline prompt)))
    (unless (null ptr)
      (unwind-protect
           (let ((str ptr))
             (when (and add-history
                        (not (sequence:emptyp str))
                        (or (not novelty-check)
                            (recent-history-line-satisfies-p
                             (std:curry novelty-check str))))
               (add-history str))
             str)
        (free-alien ptr)))))

;; (defun ensure-initialization ()
;;   "Make sure that Readline is initialized. If it's not initialized yet,
;; initialize it."
;;   (unless (find :initialized *readline-state*)
;;     (initialize)))

;; (defmacro with-possible-redirection (filename append &body body)
;;   "If FILENAME is not NIL, try to create C file named FILENAME,
;; temporarily reassign `*outstream*' to pointer to this file, perform BODY,
;; then close the file and assign `*outstream*' the old value. If APPEND is not
;; NIL, output will be appended to the file. Returns NIL on success and T on
;; failure."
;;   (std:with-gensyms (temp-outstream file-pointer body-fnc)
;;     `(flet ((,body-fnc ()
;;               ,@body))
;;        (if ,filename
;;            (let ((,temp-outstream *outstream*)
;;                  (,file-pointer (foreign-funcall "fopen"
;;                                                  :string ,filename
;;                                                  :string (if ,append "a" "w")
;;                                                  :pointer)))
;;              (if (null-alien ,file-pointer)
;;                  t
;;                  (unwind-protect
;;                       (progn
;;                         (setf *outstream* ,file-pointer)
;;                         (,body-fnc))
;;                    (foreign-funcall "fclose"
;;                                     :pointer ,file-pointer
;;                                     :boolean)
;;                    (setf *outstream* ,temp-outstream))))
;;            (,body-fnc)))))
