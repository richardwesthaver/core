;;; rt.lisp --- Regression Testing

;; 

;;; Code:
(in-package :rt)

;;; Checks
(eval-always
  (defun %test (val &optional form)
    (let ((form (macroexpand form)))
      (if val 
          (make-test-result :pass form)
          (make-test-result :fail form)))))

(defmacro is (test)
  "The DWIM Test Checker.

(is (= 1 1)) ;=> #S(TEST-RESULT :TAG :PASS :FORM (= 1 1))

If TEST returns a truthy value, return a PASS test-result, else return
a FAIL."
  `(if *testing*
       (push-result (trace! (funcall #'rt::%test ,test ',test)) *testing*)
       (trace! (funcall #'rt::%test ,test ',test))))

;; convenience functions wrapping IS
(macrolet ((defis (name op args)
             `(defmacro ,name ,args
                `(is (,',op ,,@args))))
           (defis* (name op)
             `(defmacro ,name (&rest args)
                `(is (,',op ,@args))))
           (defisn (name op)
             `(defmacro ,name (n &rest args)
                `(is (,',op ,n ,@args))))
           (defis/ (name op args)
             `(defmacro ,name ,args
                `(is (,',op ,,@(reverse args))))))
  (defis isnt not (it))
  (defisn is= =)
  (defis iseq eq (a b))
  (defis iseql eql (a b))
  (defis isequal equal (a b))
  (defis isequalp equalp (a b))
  (defis iszero zerop (n))
  (defis isempty sequence:emptyp (seq))
  (defis* isand and)
  (defis* isor or)
  (defis* isevery every)
  (defis* issome some)
  (defisn is> >)
  (defisn is< <)
  (defisn is>= >=)
  (defisn is<= <=)
  (defis/ issubtype subtypep (type obj))
  (defis/ issubclass subclassp (type obj))
  (defis/ istype typep (type obj)))

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
        `(,props ,documentation ,dec 
                 ',(if-let ((fx (getf props :fx)))
                     `((let ((*fx* (find-fixture ,fx)))
                         ,@forms))
                     forms)))
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
                 ,@props)))
       (setq *test-suite-list* (spush obj *test-suite-list* :test #'test-name=))
       obj)))

(defmacro in-suite (name)
  "Set *TEST-SUITE* to the TEST-SUITE object referred to by symbol
NAME. Return the object."
  (assert-suite name)
  `(progn
     (setq *test-suite* (ensure-suite ,name))))
