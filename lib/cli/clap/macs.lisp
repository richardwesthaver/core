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
     (if *no-debug*
         (sb-ext:disable-debugger)
         (sb-ext:enable-debugger))
     (unwind-protect
          (restart-case 
              (handler-case (progn ,@body)
                (sb-sys:interactive-interrupt (c)
                  (if *no-debug*
                      (sb-ext:exit :code 130)
                      c))
                (error (c)
                  (println c)
                  (sb-ext:exit :code 1)))
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
              (log:debug! "falling through to EXIT from pre-REPL RESTART-CASE~&")
              (exit :code 1))))
     (sb-impl::flush-standard-output-streams)
     (unless *no-exit*
       (exit :code 0))
     ;; reset terminal state
     #+nil (.ris)))

(define-constant +cli-lambda-list-keywords+ '(&rest &optional &opt &key) :test 'equal)

;; TODO 2025-01-05: env? for shell env
(defun parse-cli-lambda-list (ll)
  "Parse a specialized CLI lambda-list, returning as multiple values:

- required ARGs
- optional ARGs
- rest ARG
- OPTs
- key OPTs"
  (let ((state :required)
        (required)
        (optional)
        (rest)
        (opts)
        (key-opts))
    (labels ((%fail (l)
               (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                     l ll))
             (%check-var (l what)
               (unless (and (or (symbolp l)
                                (and (consp l) (= 2 (length l)) (symbolp (first l))))
                            (not (constantp l)))
                 (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                       what l ll)))
             (%check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (%check-var suppliedp what))))
      (dolist (l ll)
        (case l
          (&optional
           (if (eq state :required)
               (setf state l)
               (%fail l)))
          (&rest
           (if (member state '(:required &optional))
               (setf state l)
               (%fail l)))
          (&opt
           (if (member state '(:required &optional :after-rest &key))
               (setf state l)
               (%fail l)))
          (&key
           (if (member state '(:required &optional :after-rest &opt))
               (setf state l)
               (%fail l)))
          (t
           (when (member l '#.(set-difference lambda-list-keywords
                                              '(&optional &rest &key &allow-other-keys &aux &opt)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              l ll))
           (case state
             (:required
              (%check-var l "required parameter")
              (push l required))
             (&optional
              (if (consp l)
                  (destructuring-bind (name &rest tail) l
                    (%check-var name "optional parameter")
                    (cond ((cdr tail)
                           (%check-spec tail "optional-supplied-p parameter"))))
                  (%check-var l "optional parameter"))
              (push (ensure-list l) optional))
             (&opt
              (if (consp l)
                  (destructuring-bind (name &rest tail) l
                    (%check-var name "opt parameter")
                    (when (cdr tail) (%check-spec tail "opt parameter")))
                  (%check-var l "opt parameter"))
              (push (ensure-list l) opts))
             (&rest
              (%check-var l "rest parameter")
              (setf rest l
                    state :after-rest))
             (&key
              (if (consp l)
                  (destructuring-bind (var-or-kv &rest tail) l
                    (if (consp var-or-kv)
                        (destructuring-bind (keyword var) var-or-kv
                          (unless (symbolp keyword)
                            (simple-program-error "Invalid key name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                  keyword ll))
                          (%check-var var "key parameter"))
                        (%check-var var-or-kv "key parameter"))
                    (when (cdr tail)
                      (%check-spec tail "key parameter"))
                    (setf l (cons var-or-kv tail)))
                  (%check-var l "key parameter"))
              (push (ensure-list l) key-opts))
             (t (simple-program-error "invalid cli lambda-list:~%  ~S" ll)))))))
    (values (nreverse required) 
            (nreverse optional) 
            rest 
            (nreverse opts)
            (nreverse key-opts))))

#+nil (parse-cli-lambda-list '(arg1 arg2 &optional (arg3 "foo") &rest rest &opt opt1 opt2 &key key1 key2))

;; TODO 2025-09-05: Luke.. use the lambda-list, Luke..

;; DEFCMD always returns a function of two argument ARGS and OPTS - the
;; cli-lambda-list is applied to the BODY instead of closing over the
;; function.
(defmacro defcmd (name cli-lambda-list &body body)
  "Bind NAME to a functions which accepts a CLI-LAMBDA-LIST containing a
specialized lambda-list with the following keywords:

- &OPTIONAL is an optional positional argument in ARGS
- &OPT specifies a set of cli options (--foo val, -f)
- &KEY specifies a set of cli keywords (:bar val)
- &REST specifies the remainder of the ARGS passed to the CLI after all args,
  options, and keywords

CLI-LAMBDA-LIST is a list which automatically destructures and binds the
values of parsed CLI objects for the duration of BODY. The forms accepted are
the same as the SLOTS args to WITH-SLOTS - the CAR is used as the name of the
local symbol binding and the CDR is the actual name of the CLI-OPT. An atom
counts as both.

Additionally, the following special variables are bound for the duration of
BODY:

- *ARGC* : the count of arguments passed to this command
- *ARGS* : the actual list of args
- *OPTC* : the count of options passed to this command
- *OPTS* : the actual list of options"
  (multiple-value-bind (required optional rest opts keys) (parse-cli-lambda-list cli-lambda-list)
    (multiple-value-bind (body decl doc-string) (parse-body body :documentation t)
      `(defun ,name (args opts)
         ,(let ((%d '(ignorable args opts)))
            (if decl 
                (append decl (list %d))
                `(declare ,%d)))
         ,doc-string
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
                              (when-let ((val (find ,(string-downcase opt-name) *opts* 
                                                    :test 'equal
                                                    :key 'cli/clap/obj:cli-opt-name)))
                                (cli-opt-val val)))))
                 opts)
             ,@body))))))

;; DEFOPTS are much simpler - they always take a single optional argument and
;; have no lambda-list that needs to be applied.
(defmacro defopt (name &body body)
  "Define a CLI-OPT."
  (multiple-value-bind (body decl doc-string) (parse-body body :documentation t)
    `(defun ,name (&optional arg)
       ,(let ((%d '(ignorable arg)))
          (if decl 
              (append decl (list %d))
              `(declare ,%d)))
       ,@(when doc-string (list doc-string))
       (let ((*arg* arg))
         ,@body))))

(defmacro defopts (&body body)
  "Define multiple CLI-OPTs."
  (unless (null body)
    `(progn ,@(mapcar (lambda (x) `((defopt ,@x))) body))))

;; TODO 2023-10-06: 
;; (defmacro gen-cli-thunk (pvars &rest thunk)
;;   "Generate and return a function based on THUNK suitable for the :thunk
;; slot of cli objects with pandoric bindings PVARS.")

(defmacro make-opt-parser (spec &body body)
  "Return a TYPE-opt-parser function based on SPEC which is either a
symbol from *CLI-OPT-TYPES* or a list, and optional BODY which
is a list of handlers for the opt-val."
  (let* ((type (if (consp spec) (car spec) spec))
         (super (when (consp spec) (cadr spec)))
         (fn-name (symbolicate 'parse- type '-opt)))
    ;; thread em
    (let ((fn1 (unless (null super) (symbolicate "PARSE-" super "-OPT"))))
      `(defun ,fn-name (&optional arg)
         "Parse the cli-opt-val *ARG*."
         (declare (ignorable arg))
         ,@(if fn1
               `((setf *arg* (funcall #',fn1 arg)))
               `((setf *arg* arg)))
         ,@body))))
