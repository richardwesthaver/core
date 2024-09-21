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

(define-alien-type rl-hist-entry (struct rl-hist-entry
                                            (line (* t))
                                            (time (* t))
                                            (data (* t))))

;; HS_STIFLED
(define-alien-type rl-history-state
    (struct rl-history-state
            (hist-entries (array (* rl-hist-entry)))
            (offset int)
            (length int)
            (size int)
            (flags int)))

(define-alien-enum (rl-undo-code int :test eq)
                   :delete 0
                   :insert 1
                   :begin 2
                   :end 3)

(define-alien-type rl-undo-list 
  (struct rl-undo-list
          (next (* (struct rl-undo-list)))
          (start int)
          (end int)
          (text c-string)
          (what rl-undo-code)))

(define-alien-type rl-command-func
    (function int int int))

(define-alien-type rl-funmap
  (struct rl-funmap
          (name c-string)
          (function (* rl-command-func))))

(define-alien-type rl-keymap-entry
    (struct rl-keymap-entry
            (type char)
            (function (* rl-command-func))))

(define-alien-type rl-keymap (array rl-keymap-entry))

(define-alien-type readline-state 
  (struct readline-state
          (point int)
          (end int)
          (mark int)
          (buflen int)
          (buffer c-string)
          (ul (* rl-undo-list))
          (prompt c-string)
          (rlstate int)
          (done int)
          (kmap rl-keymap)
          (lastfunc (* rl-command-func))
          (insmode int)
          (edmode int)
          (kseq c-string)
          (kseqlen int)
          (pendingin int)
          (inf (* t))
          (outf (* t))
          (macro c-string)
          (catchsigs int)
          (catchsigwinch int)
          (entryfunc (* rl-command-func))
          (menuentryfunc (* rl-command-func))
          (ignorefunc (* rl-command-func))
          (attemptfunc (* rl-command-func))
          (wordbreakchars c-string)
          (reserved (array char 64))))

;;; Well Known Vars
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

;; low-level
(macrolet ((def-rl-int2 (&rest names)
             `(progn
                ,@(loop for i in names
                        collect
                           (std:with-gensyms (i1 i2)
                             `(define-alien-routine ,i int (,i1 int) (,i2 int)))))))
  (def-rl-int2 "rl_digit_argument" "rl_universal_argument" "rl_forward_byte"
    "rl_forward_char" "rl_forward" "rl_backward_byte" "rl_backward_char" "rl_backward"
    "rl_beg_of_line" "rl_end_of_line" "rl_forward_word" "rl_backward_word" "rl_refresh_line"
    "rl_clear_screen" "rl_clear_display" "rl_skip_csi_sequence" "rl_arrow_keys"
    "rl_previous_screen_line" "rl_next_screen_line"
    "rl_insert" "rl_quoted_insert" "rl_tab_insert" "rl_newline" "rl_do_lowercase_version"
    "rl_rubout" "rl_delete" "rl_rubout_or_delete" "rl_delete_horizontal_space" "rl_delete_or_show_completions"
    "rl_insert_comment" "rl_upcase_word" "rl_downcase_word" "rl_capitalize_word" "rl_transpose_words"
    "rl_transpose_chars" "rl_char_search" "rl_backward_char_search" "rl_beginning_of_history"
    "rl_end_of_history" "rl_get_next_history" "rl_get_previous_history" "rl_operate_and_get_next"
    "rl_fetch_history" "rl_set_mark" "rl_exchange_point_and_mark" "rl_vi_editing_mode"
    "rl_emacs_editing_mode" "rl_overwrite_mode" "rl_re_read_init_file" "rl_dump_functions" "rl_dump_macros"
    "rl_dump_variables" "rl_complete" "rl_possible_completions" "rl_insert_completions" "rl_old_menu_complete"
    "rl_backward_menu_complete" "rl_kill_word" "rl_backward_kill_word" "rl_kill_line" "rl_backward_kill_line"
    "rl_kill_full_line" "rl_unix_word_rubout" "rl_unix_line_discard" "rl_copy_region_to_kill" "rl_kill_region"
    "rl_copy_forward_word" "rl_copy_backward_word" "rl_yank" "rl_yank_pop" "rl_yank_nth_arg" "rl_yank_last_arg"
    "rl_bracketed_paste_begin" 
    #+win32 "rl_paste_from_clipboard"
    "rl_reverse_search_history" "rl_forward_search_history" "rl_start_kbd_macro" "rl_end_kbd_macro"
    "rl_call_last_kbd_macro" "rl_print_last_kbd_macro" "rl_revert_line" "rl_undo_command" "rl_tilde_expand"
    "rl_restart_output" "rl_stop_output" "rl_abort" "rl_tty_status" 
    "rl_history_search_forward" "rl_history_search_backward" "rl_history_substr_search_forward"
    "rl_history_substr_search_backward" "rl_noninc_forward_search" "rl_noninc_reverse_search"
    "rl_noninc_forward_search_again" "rl_noninc_reverse_search_again"
    "rl_insert_close" "rl_vi_redo" "rl_vi_undo" "rl_vi_yank_arg" "rl_vi_fetch_history" "rl_vi_search_again"
    "rl_vi_search" "rl_vi_complete" "rl_vi_tilde_expand" "rl_vi_prev_word" "rl_vi_next_word" "rl_vi_end_word"
    "rl_vi_insert_beg" "rl_vi_append_mode" "rl_vi_append_eol" "rl_vi_eof_maybe" "rl_vi_insertion_mode"
    "rl_vi_insert_mode" "rl_vi_movement_mode" "rl_vi_arg_digit" "rl_vi_change_case" "rl_vi_put" "rl_vi_column" 
    "rl_vi_delete_to" "rl_vi_change_to" "rl_vi_yank_to" "rl_vi_yank_pop" "rl_vi_rubout" "rl_vi_delete"
    "rl_vi_back_to_indent" "rl_vi_unix_word_rubout" "rl_vi_first_print" "rl_vi_char_search" "rl_vi_match" 
    "rl_vi_change_char" "rl_vi_subst" "rl_vi_overstrike" "rl_vi_overstrike_delete" "rl_vi_replace"
    "rl_vi_set_mark" "rl_vi_goto_mark" 
    ;; NOTE 2024-09-20: there are uppercase versions - fWord eWord
    "rl_vi_fword" "rl_vi_bword" "rl_vi_eword"))

;;; Well Published Functions
(define-alien-routine "readline" c-string (prompt c-string))
(define-alien-routine "rl_set_prompt" int (prompt c-string))
(define-alien-routine "rl_expand_prompt" int (prompt c-string))
(define-alien-routine "rl_initialize" int)
;; undocument; unused by readline
;; (define-alien-routine "rl_discard_argument" int)

;; [[file:/usr/include/readline/readline.h::/* Utility functions to bind keys to readline commands. */][last]]
(define-alien-routine "rl_add_defun" int (name c-string) (func (* rl-command-func)))
(define-alien-routine "rl_bind_key" int (key int) (function (* rl-command-func)))
(define-alien-routine "rl_bind_key_in_map" int (key int) (func (* rl-command-func)) (map rl-keymap))
(define-alien-routine "rl_unbind_key" int (key int))
(define-alien-routine "rl_unbind_key_in_map" int (key int) (map rl-keymap))
(define-alien-routine "rl_bind_key_if_unbound" int (key int) (function (* rl-command-func)))
(define-alien-routine "rl_bind_key_if_unbound_in_map" int (key int) (function (* rl-command-func)) (map rl-keymap))
(define-alien-routine "rl_generic_bind" int (key int) (str c-string) (name c-string) (map rl-keymap))
(define-alien-routine "rl_variable_value" c-string (name c-string))
(define-alien-routine "rl_variable_bind" int (name c-string) (val c-string))

(define-alien-routine "rl_read_init_file" int (file c-string))
(define-alien-routine "rl_parse_and_bind" int (binding c-string))

;; keymaps
(define-alien-routine "rl_make_bare_keymap" rl-keymap)
(define-alien-routine "rl_empty_keymap" int (map rl-keymap))
(define-alien-routine "rl_copy_keymap" rl-keymap (map rl-keymap))
(define-alien-routine "rl_make_keymap" rl-keymap)
(define-alien-routine "rl_discard_keymap" void (map rl-keymap))
(define-alien-routine "rl_free_keymap" void (map rl-keymap))
(define-alien-routine "rl_set_keymap" void (map rl-keymap))
(define-alien-routine "rl_get_keymap" rl-keymap)
(define-alien-routine "rl_set_keymap_name" int (name c-string) (map rl-keymap))

;; funmaps
(define-alien-routine "rl_add_funmap_entry" int (name c-string) (function (* rl-command-func)))
(define-alien-routine "rl_funmap_names" (array c-string))

;; kbd macros
(define-alien-routine "rl_push_macro_input" void (input c-string))

;; undo
(define-alien-routine "rl_add_undo" void (code rl-undo-code) (i1 int) (i2 int) (input c-string))
(define-alien-routine "rl_free_undo_list" void)
(define-alien-routine "rl_do_undo" int)
(define-alien-routine "rl_begin_undo_group" int)
(define-alien-routine "rl_end_undo_group" int)
(define-alien-routine "rl_modifying" int (i1 int) (i2 int))

;; redisplay
(define-alien-routine "rl_redisplay" void)
(define-alien-routine "rl_on_new_line" int)
(define-alien-routine "rl_on_new_line_with_prompt" int)
(define-alien-routine "rl_forced_update_display" int)
(define-alien-routine "rl_clear_visible_line" int)
(define-alien-routine "rl_clear_message" int)
(define-alien-routine "rl_reset_line_state" int)
(define-alien-routine "rl_crlf" int)

;; mark and region
(define-alien-routine "rl_keep_mark_active" void)
(define-alien-routine "rl_activate_mark" void)
(define-alien-routine "rl_deactivate_mark" void)
(define-alien-routine "rl_mark_active_p" int)
(define-alien-routine "rl_message" int)
(define-alien-routine "rl_show_char" int (char int))
;; undocumented
(define-alien-routine "rl_character_len" int (i1 int) (i2 int))
(define-alien-routine "rl_redraw_prompt_last_line" void)

(define-alien-routine "rl_save_prompt" void)
(define-alien-routine "rl_restore_prompt" void)

;; text editing
(define-alien-routine "rl_replace_line" void (line c-string) (idx int))
(define-alien-routine "rl_insert_text" int (text c-string))
(define-alien-routine "rl_delete_text" int (i1 int) (i2 int))
(define-alien-routine "rl_kill_text" int (i1 int) (i2 int))
(define-alien-routine "rl_copy_text" c-string (i1 int) (i2 int))

;; tty
(define-alien-routine "rl_prep_terminal" void (i int))
(define-alien-routine "rl_deprep_terminal" void)
(define-alien-routine "rl_tty_set_default_bindings" void (map rl-keymap))
(define-alien-routine "rl_tty_unset_default_bindings" void (map rl-keymap))
(define-alien-routine "rl_tty_set_echoing" int (val int))
(define-alien-routine "rl_reset_terminal" int (val c-string))
(define-alien-routine "rl_resize_terminal" void)
(define-alien-routine "rl_set_screen_size" void (x int) (y int))
(define-alien-routine "rl_get_screen_size" void (i1 (* int)) (i2 (* int)))
(define-alien-routine "rl_reset_screen_size" void)

(define-alien-routine "rl_get_termcap" c-string (key c-string))

;; character input
(define-alien-routine "rl_stuff_char" int (c int))
(define-alien-routine "rl_execute_next" int (i int))
(define-alien-routine "rl_clear_pending_input" int)
(define-alien-routine "rl_read_key" int)
(define-alien-routine "rl_getc" int (c (* t))) ;; NOTE: (* FILE)
(define-alien-routine "rl_set_keyboard_input_timeout" int (val int))

;;timeouts 
(define-alien-routine "rl_set_timeout" int (n1 unsigned-int) (n2 unsigned-int))
(define-alien-routine "rl_timeout_remaining" int (n1 (* unsigned-int)) (n2 (* unsigned-int)))

;; public utils
(define-alien-routine "rl_extend_lind_buffer" void (i int))
(define-alien-routine "rl_ding" int)
(define-alien-routine "rl_alphabetic" int (i int))
(define-alien-routine "rl_free" void (o (* t)))

;; signals
(define-alien-routine "rl_set_signals" int)
(define-alien-routine "rl_clear_signals" int)
(define-alien-routine "rl_cleanup_after_signal" void)
(define-alien-routine "rl_reset_after_signal" void)
(define-alien-routine "rl_free_line_state" void)
(define-alien-routine "rl_pending_signal" int)
(define-alien-routine "rl_check_signals" void)
(define-alien-routine "rl_echo_signal_char" void (c int))
(define-alien-routine "rl_set_paren_blink_timeout" int (val int))

;; history
(define-alien-routine "rl_clear_history" void)
(define-alien-routine "rl_maybe_save_line" int)
(define-alien-routine "rl_maybe_unsave_line" int)
(define-alien-routine "rl_maybe_replace_line" int)

;; completion
(define-alien-routine "rl_complete_internal" int (i int))
(define-alien-routine "rl_display_match_list" void (list (array c-string)) (i1 int) (i2 int))
;; (define-alien-routine "rl_completion_matches" (array c-string) (input c-string) (function (* rl-compentry-func)))
(define-alien-routine "rl_username_completion_function" c-string (name c-string) (i int))
(define-alien-routine "rl_filename_completion_function" c-string (name c-string) (i int))
(define-alien-routine "rl_completion_mode" int (function (* rl-command-func)))

;; state
(define-alien-routine "rl_save_state" int (state (* readline-state)))
(define-alien-routine "rl_restore_state" int (state (* readline-state)))

;; history.h
(define-alien-routine "using_history" void)
(define-alien-routine "add_history" void (line c-string))
(define-alien-routine "clear_history" void)
(define-alien-routine "stifle_history" void (i int))
(define-alien-routine "unstifle_history" int)
(define-alien-routine "history_is_stifled" int)
(define-alien-routine "history_list" (array (* rl-hist-entry)))
(define-alien-routine "previous_history" (* rl-hist-entry))
(define-alien-routine "next_history" (* rl-hist-entry))

(defvar +c-buffer-size+ 256
  "How many bytes to allocate per Lisp string when converting list of
Lisp strings into array of C strings.")

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
      (with-alien ((s rl-hist-entry))
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
