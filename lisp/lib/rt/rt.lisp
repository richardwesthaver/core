;;; rt.lisp --- Regression Testing

;; 

;;; Code:
(in-package :rt)

;;; Checks
(eval-always
  (defun %test (val &optional form)
    (let ((r
            (if val 
                (make-test-result :pass form)
                (make-test-result :fail form))))
      ;; (print r *standard-output*)
      r)))

(defmacro is (test &rest args)
  "The DWIM Check.

(is (= 1 1)) ;=> #S(TEST-RESULT :TAG :PASS :FORM (= 1 1))
If TEST returns a truthy value, return a PASS test-result, else return
a FAIL. The TEST is parameterized by ARGS which is a plist or nil.

If ARGS is nil, TEST is bound to to the RESULT slot of the test-result
and evaluated 'as-is'.

(nyi!)
ARGS may contain the following keywords followed by a corresponding
value:

:EXPECTED

:TIMEOUT

:THEN

All other values are treated as let bindings.
"
    (with-gensyms (form)
      `(if ,(null args)
           (if *testing*
               (push-result (trace! (funcall #'rt::%test ,test ',test)) *testing*)
               (trace! (funcall #'rt::%test ,test ',test)))
           (macrolet ((,form (test) `(let ,,(group args 2) ,test)))
             ;; TODO 2023-09-21: does this work...
             (if *testing*
                 (push-result (trace! (funcall #'rt::%test (,form ,test) ',test) *testing*))
                 (trace! (funcall #'rt::%test (,form ,test) ',test)))))))

;; convenience functions wrapping IS
(macrolet ((defis (name op args)
             `(defmacro ,name ,args
                `(is (,',op ,,@args))))
           (defis* (name op)
             `(defmacro ,name (&rest args)
                `(is (,',op ,@args))))
           (defisn (name op)
             `(defmacro ,name (n &rest args)
                `(is (,',op ,n ,@args)))))
  (defis isnt not (it))
  (defisn is= =)
  (defis iseq eq (a b))
  (defis iseql eql (a b))
  (defis isequal equal (a b))
  (defis iszerop zerop (n))
  (defis isemptyp sequence:emptyp (seq))
  (defis* isand and)
  (defis* isor or)
  (defis* isevery every)
  (defis* issome some)
  (defisn is> >)
  (defisn is< <)
  (defisn is>= >=)
  (defisn is<= <=)
  (defis istypep typep (type obj)))

(defmacro signals (condition-spec &body body)
  "Generates a passing TEST-RESULT if body signals a condition of type
CONDITION-SPEC. BODY is evaluated in a block named NIL, CONDITION-SPEC
is not evaluated."
  (let ((block-name (gensym)))
    (destructuring-bind (condition &optional reason-control &rest reason-args)
        (ensure-list condition-spec)
      `(block ,block-name
         (handler-bind ((,condition (lambda (c)
                                      (declare (ignore c))
                                      ;; ok, body threw condition
                                      ;; TODO 2023-09-05: result collectors
                                      ;; (add-result 'test-passed
                                      ;;            :test-expr ',condition)
                                      (return-from ,block-name (make-test-result :pass ',body)))))
           (block nil
             (locally (declare (sb-ext:muffle-conditions warning))
               ,@body)))
         (fail!
          ',condition
          ,@(if reason-control
                `(,reason-control ,@reason-args)
                `("Failed to signal a ~S" ',condition)))
         (return-from ,block-name nil)))))

;;; Macros
(defmacro deftest (name props &body body)
  "Build a test with NAME, parameterized by PROPS and with a test form of BODY.

PROPS is a plist which currently accepts the following parameters:

:PERSIST - re-run this test even if it passes

:PROFILE - enable profiling of this test

:SKIP - don't push this test to the current *TEST-SUITE*

:BENCH - enable benchmarking of this test

BODY is parsed with SB-INT:PARSE-BODY and will fill in documentation
and declarations for the test body.
"
  (destructuring-bind (pr documentation dec fn)
      (multiple-value-bind (forms dec documentation)
          ;; parse body with docstring allowed
          (parse-body (or body) :documentation t :whole t)
        `(,props ,documentation ,dec ',forms))
    ;; TODO 2023-09-21: parse plist
    `(let ((obj (make-test
                 :name ,(format nil "~A" name)
                 :form ,fn
                 ,@(when-let ((v (getf pr :persist))) `(:persist ,v))
                 ,@(when-let ((v (getf pr :bench))) `(:bench ,v))
                 ,@(when-let ((v (getf pr :profile))) `(:profile ,v))
                 ,@(when documentation `(:documentation ,documentation))
                 ,@(when dec `(:declare ,dec)))))
       ,(unless (getf pr :skip) '(push-test obj *test-suite*))
       obj)))

(defmacro defsuite (suite-name &rest props)
  "Define a TEST-SUITE with provided keys. The object returned can be
enabled using the IN-SUITE macro, similiar to the DEFPACKAGE API."
  (check-type suite-name (or symbol string))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((obj (make-suite
                 :name (format nil "~A" ',suite-name)
                 ,@(when-let ((v (getf props :stream))) `(:stream ,v)))))
       (setq *test-suite-list* (spush obj *test-suite-list* :test #'test-name=))
       obj)))

(defmacro in-suite (name)
  "Set *TEST-SUITE* to the TEST-SUITE object referred to by symbol
NAME. Return the object."
  (assert-suite name)
  `(progn
     (setq *test-suite* (ensure-suite ,name))))
