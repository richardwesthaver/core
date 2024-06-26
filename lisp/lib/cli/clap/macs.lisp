;;; cli/clap/macs.lisp --- Clap Macros

;; 

;;; Code:
(in-package :cli/clap/macs)

(defmacro argp (arg &optional (args (args)))
  "Test for presence of ARG in ARGS. Return the tail of
ARGS starting from the position of ARG."
  `(member ,arg ,args :test 'equal))

(defmacro make-shorty (name)
  "Return the first char of symbol or string NAME."
  `(character (aref (if (stringp ,name) ,name (symbol-name ,name)) 0)))

(defmacro with-cli-handlers (&body body)
  "A wrapper which handles common cli errors that may occur during
evaluation of BODY."
  `(progn
     (if *no-exit*
         (sb-ext:enable-debugger)
         (sb-ext:disable-debugger))
     (unwind-protect
          (handler-case (progn ,@body)
            (sb-sys:interactive-interrupt ()
              (println ":SIGINT")
              (sb-ext:exit :code 130)))
       ;; reset terminal state
       #+nil (.ris))))

;; TODO fix these macros
(defmacro defcmd (name &body body)
  `(defun ,name ($args $opts) 
     (declare (ignorable $args $opts))
     (let (($argc (length $args))
           ($optc (length $opts)))
       (declare (ignorable $argc $optc))
       ,@body)))

(defmacro defopt (name &body body)
  `(defun ,name (&optional $val)
     (declare (ignorable $val))
     ,@body))

(declaim (inline walk-cli-slots))
(defun walk-cli-slots (cli)
  "Walk the plist CLI, performing actions as necessary based on the slot
keys."
  (loop for kv in (group cli 2)
        when (eql :thunk (car kv))
        return (let ((th (cdr kv)))
                 (if (or (functionp th) (symbolp th)) (funcall th) (compile nil (lambda () th)))))
  cli)

;; TODO 2023-10-06: 
;; (defmacro gen-cli-thunk (pvars &rest thunk)
;;   "Generate and return a function based on THUNK suitable for the :thunk
;; slot of cli objects with pandoric bindings PVARS.")
(eval-always
  (defmacro make-opt-parser (kind-spec &body body)
    "Return a KIND-opt-parser function based on KIND-SPEC which is either a
symbol from *cli-opt-kinds* or a list, and optional BODY which
is a list of handlers for the opt-val."
    (let* ((kind (if (consp kind-spec) (car kind-spec) kind-spec))
           (super (when (consp kind-spec) (cadr kind-spec)))
           (fn-name (symbolicate 'parse- kind '-opt)))
      ;; thread em
    (let ((fn1 (unless (null super) (symbolicate "PARSE-" super "-OPT"))))
      `(defun ,fn-name ($val)
         "Parse the cli-opt-val $VAL."
         ,@(when fn1 `((setq $val (funcall #',fn1 $val))))
         ,@body)))))
