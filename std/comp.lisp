;;; comp.lisp --- Lisp Compiler Utilities

;; 

;;; Code:
(in-package :std/comp)

;; from SBCL
(defun deep-size (obj &optional (leafp (lambda (x)
                                         (typep x '(or package symbol sb-kernel:fdefn
                                                    function sb-kernel:code-component
                                                    sb-kernel:layout sb-kernel:classoid)))))
  "Compute size of OBJ including descendants. LEAFP specifies what object types
to treat as not reaching any other object. You pretty much have to treat
symbols as leaves, otherwise you reach a package and then the result just
explodes to beyond the point of being useful. (It works, but might reach the
entire heap) To turn this into an actual thing, we'd want to reduce the
consing."
  (let ((worklist (list obj))
        (seen (make-hash-table :test 'eq))
        (tot-bytes 0))
    (setf (gethash obj seen) t)
    (flet ((visit (thing)
             (when (sb-vm:is-lisp-pointer (sb-kernel:get-lisp-obj-address thing))
               (unless (or (funcall leafp thing)
                           (gethash thing seen))
                 (push thing worklist)
                 (setf (gethash thing seen) t)))))
      (loop
        (unless worklist (return))
        (let ((x (pop worklist)))
          (incf tot-bytes (primitive-object-size x))
          (sb-vm:do-referenced-object (x visit)))))
    ;; Secondary values is number of visited objects not incl. original one.
    (values tot-bytes
            (1- (hash-table-count seen))
            seen)))

(defun get-simple-fun-instruction-model (fun)
  (declare (type sb-kernel:simple-fun fun))
  (sb-disassem:get-inst-space) ; for effect
  (let* ((code (sb-kernel:fun-code-header fun))
         (segment (sb-disassem:make-code-segment code
                                                 (sb-sys:sap- (sb-vm:simple-fun-entry-sap fun)
                                                              (sb-kernel:code-instructions code))
                                                 (sb-kernel:%simple-fun-text-len fun)))
         (dstate (sb-disassem:make-dstate nil)))
    (setf (sb-disassem::dstate-absolutize-jumps dstate) nil
          (sb-disassem:dstate-segment dstate) segment
          (sb-disassem:dstate-segment-sap dstate) (funcall (sb-disassem:seg-sap-maker segment)))
    (sb-int:collect ((result))
      (loop (let ((pc (sb-disassem:dstate-cur-offs dstate)))
              (result (cons pc (sb-disassem:disassemble-instruction dstate))))
            (when (>= (sb-disassem:dstate-cur-offs dstate) (sb-disassem:seg-length segment))
              (return)))
      (result))))

(defun %asm (instructions)
  "Take a list of lists and assemble them as though they are instructions inside
the body of a vop. There is no need to use the INST macro in front of each
list. As a special case, if an atom is the symbol LABEL, it will be changed to
a generated label. At most one such atom may appear."
  (let ((segment (sb-assem:make-segment))
        (label))
    (sb-assem:assemble (segment 'nil)
      (dolist (inst instructions)
        (setq inst (copy-list inst))
        (mapl (lambda (cell &aux (x (car cell)))
                (when (and (symbolp x) (string= x "LABEL"))
                  (setq label (sb-assem:gen-label))
                  (rplaca cell label)))
              inst)
        (apply #'sb-assem:inst* (car inst) (cdr inst)))
      (when label
        (sb-assem::%emit-label segment nil label)))
    (sb-assem:segment-buffer
     (sb-assem:finalize-segment segment))))

(defun prepare-form (thing &key optimize)
  (cond
    ((functionp thing)
     (error "~@<~S is a function, not a form.~@:>" thing))
    ((not optimize)
     thing)
    ((typep thing '(cons (eql sb-int:named-lambda)))
     `(,@(subseq thing 0 3)
       (declare (optimize ,@optimize))
       ,@(nthcdr 3 thing)))
    ((typep thing '(cons (eql lambda)))
     `(,(first thing) ,(second thing)
       (declare (optimize ,@optimize))
       ,@(nthcdr 2 thing)))
    (t
     (error "~@<Cannot splice ~A declaration into forms other than ~
             ~{~S~#[~; and ~:;, ~]~}: ~S.~@:>"
            'optimize '(lambda sb-int:named-lambda) thing))))

(defun compile-capturing-output-and-conditions
    (form &key name condition-transform)
  (let ((warnings '())
        (style-warnings '())
        (notes '())
        (compiler-errors '())
        (error-output (make-string-output-stream)))
    (flet ((maybe-transform (condition)
             (if condition-transform
                 (funcall condition-transform condition)
                 condition)))
      (handler-bind ((sb-ext:compiler-note
                       (lambda (condition)
                         (push (maybe-transform condition) notes)
                         (muffle-warning condition)))
                     (style-warning
                       (lambda (condition)
                         (push (maybe-transform condition) style-warnings)
                         (muffle-warning condition)))
                     (warning
                       (lambda (condition)
                         (push (maybe-transform condition) warnings)
                         (muffle-warning condition)))
                     (sb-c:compiler-error
                       (lambda (condition)
                         (push (maybe-transform condition) compiler-errors))))
        (multiple-value-bind (function warnings-p failure-p)
            (let ((*error-output* error-output))
              (compile name form))
          (values function warnings-p failure-p
                  warnings style-warnings notes compiler-errors
                  error-output))))))

(defun print-form-and-optimize (stream form-and-optimize &optional colonp atp)
  (declare (ignore colonp atp))
  (destructuring-bind (form . optimize) form-and-optimize
    (format stream "~@:_~@:_~2@T~S~@:_~@:_~
                    with ~:[~
                      default optimization policy ~
                    ~;~
                      ~:*~@:_~@:_~2@T~S~@:_~@:_~
                      optimization policy~
                    ~]"
            form optimize)))

(defun print-signaled-conditions (stream conditions &optional colonp atp)
  (declare (ignore colonp atp))
  (format stream "~{~@:_~@:_~{~/sb-ext:print-symbol-with-prefix/: ~A~}~}"
          (mapcar (lambda (condition)
                    (list (type-of condition) condition))
                  conditions)))

(defun checked-compile (form
                        &key
                        name
                        allow-failure
                        allow-warnings
                        allow-style-warnings
                        (allow-notes t)
                        (allow-compiler-errors allow-failure)
                        condition-transform
                        optimize)
  "Compile FORM capturing and muffling all [style-]warnings and notes and
return six values: 1) the compiled function 2) a Boolean indicating whether
compilation failed 3) a list of warnings 4) a list of style-warnings 5) a list
of notes 6) a list of SB-C:COMPILER-ERROR conditions.

An error can be signaled when COMPILE indicates failure as well as in case
[style-]warning or note conditions are signaled. The keyword parameters
ALLOW-{FAILURE,[STYLE-]WARNINGS,NOTES,COMPILER-ERRORS} control this
behavior. All but ALLOW-NOTES default to NIL.

Arguments to the ALLOW-{FAILURE,[STYLE-]WARNINGS,NOTES,COMPILER-ERRORS}
keyword parameters are interpreted as type specifiers restricting the allowed
conditions of the respective kind.

When supplied, the value of CONDITION-TRANSFORM has to be a function of one
argument, the condition currently being captured. The returned value is
captured and later returned in place of the condition."
  (sb-int:binding* ((prepared-form (prepare-form form :optimize optimize))
                    ((function nil failure-p
                         warnings style-warnings notes compiler-errors
                         error-output)
                     (compile-capturing-output-and-conditions
                      prepared-form :name name :condition-transform condition-transform)))
    (labels ((fail (kind conditions &optional allowed-type)
               (error "~@<Compilation of ~/std::print-form-and-optimize/~
                       signaled ~A~P:~/std::print-signaled-conditions/~
                       ~@[~@:_~@:_Allowed type is ~
                      ~/sb-impl:print-type-specifier/.~]~@:>"
                      (cons form optimize) kind (length conditions) conditions
                      allowed-type))
             (check-conditions (kind conditions allow)
               (cond
                 (allow
                  (let ((offenders (remove-if (lambda (condition)
                                                (typep condition allow))
                                              conditions)))
                    (when offenders
                      (fail kind offenders allow))))
                 (conditions
                  (fail kind conditions)))))
      (when (and (not allow-failure) failure-p)
        (let ((output (get-output-stream-string error-output)))
          (error "~@<Compilation of~/std::print-form-and-optimize/ ~
                  failed~@[ with output~
                  ~@:_~@:_~2@T~@<~@;~A~:>~@:_~@:_~].~@:>"
                 (cons form optimize) (when (plusp (length output)) output))))

      (check-conditions "warning"        warnings        allow-warnings)
      (check-conditions "style-warning"  style-warnings  allow-style-warnings)
      (check-conditions "note"           notes           allow-notes)
      (check-conditions "compiler-error" compiler-errors allow-compiler-errors)

      ;; Since we may have prevented warnings from being taken
      ;; into account for FAILURE-P by muffling them, adjust the
      ;; second return value accordingly.
      (values function (when (or failure-p warnings) t)
              warnings style-warnings notes compiler-errors))))

(defun print-arguments (stream arguments &optional colonp atp)
  (declare (ignore colonp atp))
  (format stream "~:[~
                    without arguments ~
                  ~;~:*~
                    with arguments~@:_~@:_~
                    ~2@T~@<~{~S~^~@:_~}~:>~@:_~@:_~
                  ~]"
          arguments))

(defun call-capturing-values-and-conditions (function &rest args)
  (let ((values nil)
        (conditions '()))
    (block nil
      (handler-bind ((condition (lambda (condition)
                                  (push condition conditions)
                                  (typecase condition
                                    (warning
                                     (muffle-warning condition))
                                    (serious-condition
                                     (return))))))
        (setf values (multiple-value-list (apply function args)))))
    (values values (nreverse conditions))))

(defun type-specifiers-equal (left right)
  (let ((a (sb-kernel:values-specifier-type left)))
    ;; SPECIFIER-TYPE is a memoized function, and TYPE= is a trivial
    ;; operation if A and B are EQ.
    ;; To actually exercise the type operation, remove the memoized parse.
    (sb-int:drop-all-hash-caches)
    (let ((b (sb-kernel:values-specifier-type right)))
      (sb-kernel:type= a b))))

(defun %checked-compile-and-assert-one-case
    (form optimize function args-thunk expected test allow-conditions)
  (if (eq args-thunk :return-type)
      (let ((type (sb-kernel:%simple-fun-type function)))
        (unless (or (eq type 'function)
                    #.(and (not (member :unwind-to-frame-and-call-vop sb-impl:+internal-features+))
                           '(member '(debug 3) optimize :test #'equal))
                    (type-specifiers-equal (caddr type) expected))
          (error "~@<The derived type of~
                   ~/print-form-and-optimize/ ~
                   is ~/sb-impl:print-type-specifier/
                   while
                    ~/sb-impl:print-type-specifier/
                   is expected~@:>"
                 (cons form optimize) type expected)))
      (let ((args (multiple-value-list (funcall args-thunk))))
        (flet ((failed-to-signal (expected-type)
                 (error "~@<Calling the result of compiling~
                      ~/std::print-form-and-optimize/ ~
                      ~/std::print-arguments/~
                      returned normally instead of signaling a ~
                      condition of type ~
                      ~/sb-impl:print-type-specifier/.~@:>"
                        (cons form optimize) args expected-type))
               (signaled-unexpected (conditions)
                 (error "~@<Calling the result of compiling~
                      ~/std::print-form-and-optimize/ ~
                      ~/std::print-arguments/~
                      signaled unexpected condition~P~
                      ~/print-signaled-conditions/~
                      .~@:>"
                        (cons form optimize) args (length conditions) conditions))
               (returned-unexpected (values expected test)
                 (error "~@<Calling the result of compiling~
                     ~/std::print-form-and-optimize/ ~
                     ~/std::print-arguments/~
                     returned values~@:_~@:_~
                     ~2@T~<~{~S~^~@:_~}~:>~@:_~@:_~
                     which is not ~S to~@:_~@:_~
                     ~2@T~<~{~S~^~@:_~}~:>~@:_~@:_~
                     .~@:>"
                        (cons form optimize) args
                        (list values) test (list expected))))
          (multiple-value-bind (values conditions)
              (apply #'call-capturing-values-and-conditions function args)
            (typecase expected
              ((cons (eql condition) (cons t null))
               (let* ((expected-condition-type (second expected))
                      (unexpected (remove-if (lambda (condition)
                                               (typep condition
                                                      expected-condition-type))
                                             conditions))
                      (expected (set-difference conditions unexpected)))
                 (cond
                   (unexpected
                    (signaled-unexpected unexpected))
                   ((null expected)
                    (failed-to-signal expected-condition-type)))))
              (t
               (let ((expected (funcall expected)))
                 (cond
                   ((and conditions
                         (not (and allow-conditions
                                   (every (lambda (condition)
                                            (typep condition allow-conditions))
                                          conditions))))
                    (signaled-unexpected conditions))
                   ((not (funcall test values expected))
                    (returned-unexpected values expected test)))))))))))

(defun %checked-compile-and-assert-one-compilation
    (form optimize other-checked-compile-args cases)
  (let ((function (apply #'checked-compile form
                         (if optimize
                             (list* :optimize optimize
                                    other-checked-compile-args)
                             other-checked-compile-args))))
    (loop for (args-thunk values test allow-conditions) in cases
          do (%checked-compile-and-assert-one-case
              form optimize function args-thunk values test allow-conditions))))

;;; Optimization Qualities
(sb-int:defconstant-eqx +optimization-quality-names+
    '(speed safety debug compilation-speed space) #'equal)

(sb-int:defconstant-eqx +optimization-quality-keywords+
    '(:speed :safety :debug :compilation-speed :space) #'equal)

(declaim (ftype (function #.`(function
                                 &key
                                 ,@(mapcar #'list +optimization-quality-keywords+
                                           '#1=(optimization-quality-range-designator . #1#))
                                 (filter function)))
                map-optimization-quality-combinations
                map-optimize-declarations))

(deftype optimization-quality-range-designator ()
  '(or (eql nil)                                ; skip quality
    (integer 0 3)                            ; one value
    (cons (or (eql nil) (integer 0 3)) list) ; list of values, nil means skip
    (eql t)))                                ; all values

(defun map-optimization-quality-combinations
    (function &key (speed t) (safety t) (debug t) (compilation-speed t) (space t)
              filter)
  (labels ((map-quantity-values (values thunk)
             (typecase values
               ((eql t)
                (dotimes (i 4) (funcall thunk i)))
               (cons
                (map nil thunk values))
               ((integer 0 3)
                (funcall thunk values))))
           (one-quality (qualities specs values)
             (let ((quality (first qualities))
                   (spec    (first specs)))
               (cond
                 ((not quality)
                  (when (or (not filter) (apply filter values))
                    (apply function values)))
                 ((not spec)
                  (one-quality (rest qualities) (rest specs) values))
                 (t
                  (map-quantity-values
                   spec
                   (lambda (value)
                     (one-quality (rest qualities) (rest specs)
                                  (if value
                                      (list* quality value values)
                                      values)))))))))
    (one-quality +optimization-quality-keywords+
                 (list speed safety debug compilation-speed space)
                 '())))

(defun map-optimize-declarations
    (function &rest args
        &key speed safety debug compilation-speed space filter)
  (declare (ignore speed safety debug compilation-speed space filter))
  (apply #'map-optimization-quality-combinations
         (lambda (&rest args &key &allow-other-keys)
           (funcall function (loop for name in +optimization-quality-names+
                                   for keyword in +optimization-quality-keywords+
                                   for value = (getf args keyword)
                                   when value collect (list name value))))
         args))

(defun expand-optimize-specifier (specifier)
  (etypecase specifier
    (cons
     specifier)
    ((eql nil)
     '(:speed nil :safety nil :debug nil :compilation-speed nil :space nil))
    ((eql :default)
     '(:speed 1 :safety 1 :debug 1 :compilation-speed 1 :space 1))
    ((eql :maximally-safe)
     (list :filter (lambda (&key safety &allow-other-keys)
                     (= safety 3))))
    ((eql :safe)
     (list :filter (lambda (&key speed safety &allow-other-keys)
                     (and (> safety 0) (>= safety speed)))
           :compilation-speed 1 :space 1))
    ((eql :quick)
     '(:compilation-speed 1 :space 1))
    ((eql :quick/incomplete)
     '(:compilation-speed nil :space nil))
    ((eql :all)
     '())))

(defun map-optimize-declarations* (function specifier)
  (apply #'map-optimize-declarations
         function (expand-optimize-specifier specifier)))

;;; Checked Compile
(defun %checked-compile-and-assert (form checked-compile-args cases)
  (let ((optimize (getf checked-compile-args :optimize))
        (other-args (loop for (key value) on checked-compile-args by #'cddr
                          unless (eq key :optimize)
                          collect key and collect value)))
    (map-optimize-declarations*
     (lambda (&optional optimize)
       (%checked-compile-and-assert-one-compilation
        form optimize other-args cases))
     optimize)))

;; Compile FORM using CHECKED-COMPILE, then call the resulting
;; function with arguments and assert expected return values
;; according to CASES.
;;
;; Elements of CASES are of the form
;;
;;   ((&rest ARGUMENT-FORMS) VALUES-FORM &key TEST ALLOW-CONDITIONS)
;;
;; where ARGUMENT-FORMS are evaluated to produce the arguments for
;; one call of the function and VALUES-FORM is evaluated to produce
;; the expected return values for that function call.
;;
;; TEST is used to compare a list of the values returned by the
;; function call to the list of values obtained by calling
;; VALUES-FORM.
;;
;; If supplied, the value of ALLOW-CONDITIONS is a type-specifier
;; indicating which conditions should be allowed (and ignored) during
;; the function call.
;;
;; If VALUES-FORM is of the form
;;
;;   (CONDITION CONDITION-TYPE)
;;
;; the function call is expected to signal the designated condition
;; instead of returning values. CONDITION-TYPE is evaluated.
;;
;; The OPTIMIZE keyword parameter controls the optimization policies
;; (or policy) used when compiling FORM. The argument is interpreted
;; as described for MAP-OPTIMIZE-DECLARATIONS*.
;;
;; The other keyword parameters, NAME and
;; ALLOW-{WARNINGS,STYLE-WARNINGS,NOTES}, behave as with
;; CHECKED-COMPILE.
(defmacro checked-compile-and-assert ((&key name
                                            allow-warnings
                                            allow-style-warnings
                                            (allow-notes t)
                                            (optimize :quick))
                                      form &body cases)
  (flet ((make-case-form (case)
           (if (typep case '(cons (member :return-type)))
               `',case
               (destructuring-bind (args values &key (test ''equal testp)
                                                     allow-conditions)
                   case
                 (let ((conditionp (typep values '(cons (eql condition) (cons t null)))))
                   (when (and testp conditionp)
                     (sb-ext:with-current-source-form (case)
                       (error "~@<Cannot use ~S with ~S ~S.~@:>"
                              values :test test)))
                   `(list (lambda () (values ,@args))
                          ,(if conditionp
                               `(list 'condition ,(second values))
                               `(lambda () (multiple-value-list ,values)))
                          ,test
                          ,allow-conditions))))))
    `(%checked-compile-and-assert
      ,form (list :name ,name
                  :allow-warnings ,allow-warnings
                  :allow-style-warnings ,allow-style-warnings
                  :allow-notes ,allow-notes
                  :optimize ,optimize)
      (list ,@(mapcar #'make-case-form cases)))))

;;; Like CHECKED-COMPILE, but for each captured condition, capture and
;;; later return a cons
;;;
;;;   (CONDITION . SOURCE-PATH)
;;;
;;; instead. SOURCE-PATH is the path of the source form associated to
;;; CONDITION.
(defun checked-compile-capturing-source-paths (form &rest args)
  (labels ((context-source-path ()
             (let ((context (sb-c::find-error-context nil)))
               (sb-c::compiler-error-context-original-source-path
                context)))
           (add-source-path (condition)
             (cons condition (context-source-path))))
    (apply #'checked-compile form :condition-transform #'add-source-path
           args)))

;;; Similar to CHECKED-COMPILE, but allow compilation failure and
;;; warnings and only return source paths associated to those
;;; conditions.
(defun checked-compile-condition-source-paths (form)
  (let ((source-paths '()))
    (labels ((context-source-path ()
               (let ((context (sb-c::find-error-context nil)))
                 (sb-c::compiler-error-context-original-source-path
                  context)))
             (push-source-path (condition)
               (declare (ignore condition))
               (push (context-source-path) source-paths)))
      (checked-compile form
                       :allow-failure t
                       :allow-warnings t
                       :allow-style-warnings t
                       :condition-transform #'push-source-path))
    (nreverse source-paths)))

;;; Runtime
(defun runtime* (thunk repetitions precision)
  "Repeat calling THUNK until its cumulated runtime, measured using
GET-INTERNAL-RUN-TIME, is larger than PRECISION. Repeat this REPETITIONS many
times and return the time one call to THUNK took in seconds as a float,
according to the minimum of the cumulated runtimes over the repetitions.

This allows to easily measure the runtime of expressions that take much less
time than one internal time unit. Also, the results are unaffected, modulo
quantization effects, by changes to INTERNAL-TIME-UNITS-PER-SECOND.

Taking the minimum is intended to reduce the error introduced by garbage
collections occurring at unpredictable times. The inner loop doubles the
number of calls to THUNK each time before again measuring the time spent, so
that the time measurement overhead doesn't distort the result if calling THUNK
takes very little time."
  (loop repeat repetitions
        minimize
           (loop with start = (get-internal-run-time)
                 with duration = 0
                 for n = 1 then (* n 2)
                 for total-runs = n then (+ total-runs n)
                 for gc-start = sb-ext:*gc-run-time*
                 do (dotimes (i n)
                      (funcall thunk))
                    (setf duration (- (get-internal-run-time) start
                                      (- sb-ext:*gc-run-time* gc-start)))
                 when (> duration precision)
                 return (/ (float duration)
                           (float total-runs)))
        into min-internal-time-units-per-call
        finally (return (/ min-internal-time-units-per-call
                           (float internal-time-units-per-second)))))

(defmacro runtime (form &key (repetitions 5) (precision (* 30
                                                           (/ internal-time-units-per-second 1000))))
  `(runtime* (lambda () ,form) ,repetitions ,precision))

;;; ASM
(defun asm-search (expect lambda)
  (let* ((code (etypecase lambda
                 (cons (checked-compile lambda))
                 (function lambda)))
         (disassembly
           (with-output-to-string (s)
             (let ((sb-disassem:*disassem-location-column-width* 0)
                   (*print-pretty* nil))
               (sb-c:dis code s)))))
    (loop for line in (std/string:lines disassembly)
          when (and (search expect line)
                    (not (search "; Origin" line)))
          collect line)))

(defun inspect-ir (form fun &rest checked-compile-args)
  (let ((*compile-component-hook* fun))
    (apply #'checked-compile form checked-compile-args)))

;;; Compile Failures
(define-condition compile-condition (condition)
  ((context-format
    :initform nil :reader compile-condition-context-format :initarg :context-format)
   (context-arguments
    :initform nil :reader compile-condition-context-arguments :initarg :context-arguments)
   (description
    :initform nil :reader compile-condition-description :initarg :description))
  (:report (lambda (c s)
             (format s "~@<~A~@[ while ~?~]~@:>"
                     (or (compile-condition-description c) (type-of c))
                     (compile-condition-context-format c)
                     (compile-condition-context-arguments c)))))
(define-condition compile-file-error (compile-condition error) ())
(define-condition compile-warned-warning (compile-condition warning) ())
(define-condition compile-warned-error (compile-condition error) ())
(define-condition compile-failed-warning (compile-condition warning) ())
(define-condition compile-failed-error (compile-condition error) ())

(declaim ((member :warn :error :ignore) *compile-file-failure-action* *compile-file-warning-action*))
(defvar *compile-file-failure-action* :error)
(defvar *compile-file-warning-action* :warn)

(defun check-lisp-compile-warnings (warnings-p failure-p
                                    &optional context-format context-arguments)
  "Given the warnings or failures as resulted from COMPILE-FILE or checking deferred warnings,
raise an error or warning as appropriate"
  (when failure-p
    (case *compile-file-failure-action*
      (:warn (warn 'compile-failed-warning
                   :description "Lisp compilation failed"
                   :context-format context-format
                   :context-arguments context-arguments))
      (:error (error 'compile-failed-error
                     :description "Lisp compilation failed"
                     :context-format context-format
                     :context-arguments context-arguments))
      (:ignore nil)))
  (when warnings-p
    (case *compile-file-warning-action*
      (:warn (warn 'compile-warned-warning
                   :description "Lisp compilation had style-warnings"
                   :context-format context-format
                   :context-arguments context-arguments))
      (:error (error 'compile-warned-error
                     :description "Lisp compilation had style-warnings"
                     :context-format context-format
                     :context-arguments context-arguments))
      (:ignore nil))))

(defun check-lisp-compile-results (output warnings-p failure-p
                                   &optional context-format context-arguments)
  "Given the results of COMPILE-FILE, raise an error or warning as appropriate"
  (unless output
    (error 'compile-file-error :context-format context-format :context-arguments context-arguments))
  (check-lisp-compile-warnings warnings-p failure-p context-format context-arguments))

;;; Deferred Warnings
(defun reify-undefined-warning (warning)
  ;; Extracting undefined-warnings from the compilation-unit
  ;; To be passed through the above reify/unreify link, it must be a "simple-sexp"
  (list*
   (sb-c::undefined-warning-kind warning)
   (sb-c::undefined-warning-name warning)
   (sb-c::undefined-warning-count warning)
   (mapcar
    #'(lambda (frob)
        ;; the lexenv slot can be ignored for reporting purposes
        `(:enclosing-source ,(sb-c::compiler-error-context-enclosing-source frob)
          :source ,(sb-c::compiler-error-context-source frob)
          :original-source ,(sb-c::compiler-error-context-original-source frob)
          :context ,(sb-c::compiler-error-context-context frob)
          :file-name ,(sb-c::compiler-error-context-file-name frob) ; a pathname
          :file-position ,(sb-c::compiler-error-context-file-position frob) ; an integer
          :original-source-path ,(sb-c::compiler-error-context-original-source-path frob)))
    (sb-c::undefined-warning-warnings warning))))

(defun reify-deferred-warnings ()
  "return a portable S-expression, portably readable and writeable in any Common Lisp implementation
using READ within a WITH-SAFE-IO-SYNTAX, that represents the warnings currently deferred by
WITH-COMPILATION-UNIT. One of three functions required for deferred-warnings support in ASDF."
  (when sb-c::*in-compilation-unit*
    ;; Try to send nothing through the pipe if nothing needs to be accumulated
    `(,@(when sb-c::*undefined-warnings*
          `((sb-c::*undefined-warnings*
             ,@(mapcar #'reify-undefined-warning sb-c::*undefined-warnings*))))
      ,@(loop :for what :in '(sb-c::*aborted-compilation-unit-count*
                              sb-c::*compiler-error-count*
                              sb-c::*compiler-warning-count*
                              sb-c::*compiler-style-warning-count*
                              sb-c::*compiler-note-count*)
              :for value = (symbol-value what)
              :when (plusp value)
              :collect `(,what . ,value)))))

(defun unreify-deferred-warnings (reified-deferred-warnings)
  "given a S-expression created by REIFY-DEFERRED-WARNINGS, reinstantiate the corresponding
deferred warnings as to be handled at the end of the current WITH-COMPILATION-UNIT.
Handle any warning that has been resolved already,
such as an undefined function that has been defined since.
One of three functions required for deferred-warnings support in ASDF."
  (declare (ignorable reified-deferred-warnings))
  (dolist (item reified-deferred-warnings)
    ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
    ;; For *undefined-warnings*, the adjustment is a list of initargs.
    ;; For everything else, it's an integer.
    (destructuring-bind (symbol . adjustment) item
      (case symbol
        ((sb-c::*undefined-warnings*)
         (setf sb-c::*undefined-warnings*
               (nconc (mapcan
                       #'(lambda (stuff)
                           (destructuring-bind (kind name count . rest) stuff
                             (unless (case kind (:function (fboundp name)))
                               (list
                                (sb-c::make-undefined-warning
                                 :name name
                                 :kind kind
                                 :count count
                                 :warnings
                                 (mapcar #'(lambda (x)
                                             (apply #'sb-c::make-compiler-error-context x))
                                         rest))))))
                       adjustment)
                      sb-c::*undefined-warnings*)))
        (otherwise
         (set symbol (+ (symbol-value symbol) adjustment)))))))

(defun reset-deferred-warnings ()
  "Reset the set of deferred warnings to be handled at the end of the current
WITH-COMPILATION-UNIT."
  (when sb-c::*in-compilation-unit*
    (setf sb-c::*undefined-warnings* nil
          sb-c::*aborted-compilation-unit-count* 0
          sb-c::*compiler-error-count* 0
          sb-c::*compiler-warning-count* 0
          sb-c::*compiler-style-warning-count* 0
          sb-c::*compiler-note-count* 0)))

(defun save-deferred-warnings (warnings-file)
  "Save forward reference conditions so they may be issued at a latter time,
possibly in a different process."
  (with-open-file (s warnings-file :direction :output :if-exists :supersede
                                   :element-type 'character
                                   :external-format :utf-8)
    (with-safe-io-syntax (:std)
      (let ((*read-eval* t))
        (write (reify-deferred-warnings) :stream s :pretty t :readably t)))
    (terpri s)))

(defun check-deferred-warnings (files &optional context-format context-arguments)
  "Given a list of FILES containing deferred warnings saved by CALL-WITH-SAVED-DEFERRED-WARNINGS,
re-intern and raise any warnings that are still meaningful."
  (let ((file-errors nil)
        (failure-p nil)
        (warnings-p nil))
    (handler-bind
        ((warning #'(lambda (c)
                      (setf warnings-p t)
                      (unless (typep c 'style-warning)
                        (setf failure-p t)))))
      (with-compilation-unit (:override t)
        (reset-deferred-warnings)
        (dolist (file files)
          (unreify-deferred-warnings
           (handler-case
               (with-safe-io-syntax ()
                 (let ((*read-eval* t))
                   (read-lisp-file file)))
             (error (c)
               ;;(delete-file-if-exists file) ;; deleting forces rebuild but prevents debugging
               (push c file-errors)
               nil))))))
    (dolist (error file-errors) (error error))
    (check-lisp-compile-warnings
     (or failure-p warnings-p) failure-p context-format context-arguments)))

(defun call-with-saved-deferred-warnings (thunk warnings-file &key source-namestring)
  "If WARNINGS-FILE is not nil, record the deferred-warnings around a call to THUNK
and save those warnings to the given file for latter use,
possibly in a different process. Otherwise just call THUNK."
  (declare (ignorable source-namestring))
  (if warnings-file
      (with-compilation-unit (:override t #+sbcl :source-namestring #+sbcl source-namestring)
        (unwind-protect
             (let (#+sbcl (sb-c::*undefined-warnings* nil))
               (multiple-value-prog1
                   (funcall thunk)
                 (save-deferred-warnings warnings-file)))
          (reset-deferred-warnings)))
      (funcall thunk)))

(defmacro with-saved-deferred-warnings ((warnings-file &key source-namestring) &body body)
  "Trivial syntax for CALL-WITH-SAVED-DEFERRED-WARNINGS"
  `(call-with-saved-deferred-warnings
    #'(lambda () ,@body) ,warnings-file :source-namestring ,source-namestring))

(defun checked-compile-file (path &rest args &key warnings-file &allow-other-keys)
  (remf args :warnings-file)
  (with-saved-deferred-warnings (warnings-file)
    (apply 'compile-file path args)))
