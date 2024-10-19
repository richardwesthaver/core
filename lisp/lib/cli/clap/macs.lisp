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
          (restart-case
              (progn ,@body)
            (sb-sys:interactive-interrupt ()
              (println ":SIGINT")
              (sb-ext:exit :code 130))
            (abort ()
              :report (lambda (s)
                        (write-string
                         "Skip to toplevel READ/EVAL/PRINT loop."
                         s)
                        (log:debug! "CONTINUEing from pre-REPL RESTART-CASE")
                        (values)))
            (exit ()
              :report "Exit SBCL (calling #'EXIT, killing the process)."
              ;; :test (lambda (c) (declare (ignore c)) t)
              (log:debug! "falling through to EXIT from pre-REPL RESTART-CASE")
              (exit :code 1))))
     (sb-impl::flush-standard-output-streams)
       ;; reset terminal state
       #+nil (.ris)))

(defmacro defcmd (name opt-list &body body)
  "Bind NAME to a functions which accepts two lists as arguments - ARGS
containing a list of strings parsed at the CLI and OPTS containg a list of
CLI-OPTs. The following special variables are bound for the duration of BODY:

- *ARGC* : the count of arguments passed to this command
- *ARGS* : the actual list of args
- *OPTC* : the count of options passed to this command
- *OPTS* : the actual list of options

OPT-LIST is a which automatically selects and binds the values of parsed
CLI-OPTs to a name via SYMBOL-MACROLET. The forms accepted are the same as the
SLOTS args to WITH-SLOTS - the CAR is used as the name of the local symbol
binding and the CDR is the actual name of the CLI-OPT."
  `(defun ,name (args opts)
     (declare (ignorable args opts)
              (sequence args opts))
     (let ((*argc* (length args))
           (*optc* (length opts))
           (*args* args)
           (*opts* opts))
       (symbol-macrolet
           ,(mapcar (lambda (x)
                      (unless (typep x
                                     '(or symbol
                                       (cons symbol (cons symbol null))))
                        (error "Malformed CLI-OPT binding: ~s, should either a symbol or (variable-name opt-name)" x))
                      (destructuring-bind (name &optional (opt-name name)) (ensure-list x)
                        `(,name
                   (cli-opt-val (find-opt ',opt-name ,*opts*)))))
             opt-list)
       ,@body))))

(defmacro defopt (name &body body)
  `(defun ,name (&optional arg)
     (let ((*arg* arg))
       ,@body)))

;; TODO 2023-10-06: 
;; (defmacro gen-cli-thunk (pvars &rest thunk)
;;   "Generate and return a function based on THUNK suitable for the :thunk
;; slot of cli objects with pandoric bindings PVARS.")
(eval-always
  (defmacro make-opt-parser (kind-spec &body body)
    "Return a KIND-opt-parser function based on KIND-SPEC which is either a
symbol from *CLI-OPT-KINDS* or a list, and optional BODY which
is a list of handlers for the opt-val."
    (let* ((kind (if (consp kind-spec) (car kind-spec) kind-spec))
           (super (when (consp kind-spec) (cadr kind-spec)))
           (fn-name (symbolicate 'parse- kind '-opt)))
      ;; thread em
    (let ((fn1 (unless (null super) (symbolicate "PARSE-" super "-OPT"))))
      `(defun ,fn-name (&optional arg)
         "Parse the cli-opt-val *ARG*."
         (declare (ignorable arg))
         ,@(if fn1
               `((setf *arg* (print (funcall #',fn1 arg))))
               `((setf *arg* arg)))
         ,@body)))))
