;;; obj.lisp --- Test Objects

;; 

;;; Code:
(in-package :rt)

;;; Result
(deftype result-tag ()
  '(or (member :pass :fail) null))

(declaim (inline %make-test-result))
(defstruct (test-result (:constructor %make-test-result)
                        (:conc-name  tr-))
  (tag nil :type result-tag :read-only t)
  (form nil :type form))

(defun make-test-result (tag &optional form)
  (%make-test-result :tag tag :form form))

(defmethod test-pass-p ((res test-result))
  (when (eq :pass (tr-tag res)) t))

(defmethod test-fail-p ((res test-result))
  (when (eq :fail (tr-tag res)) t))

(defmethod print-object ((self test-result) stream)
  (print-unreadable-object (self stream)
    (format stream "~A ~A"
            (tr-tag self)
            (tr-form self))))

;;; Test Object
(defclass test-object ()
  ((name :initarg :name :initform (required-argument) :type string :accessor name)
   #+nil (cached :initarg :cache :allocation :class :accessor test-cached-p :type boolean))
  (:documentation "Super class for all test-related objects."))

(defmethod print-object ((self test-object) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A"
            (name self))))

;;; Fixtures
;; TODO 2026-01-20: 
;; Our fixtures are objects which can be inherited to build different fixture
;; classes. Fixtures inherit from TEST-OBJECT and have a NAME which usually
;; indicates the key used to initialize this object with MAKE-FIXTURE.

;; You can use fixtures inside a test or use the push-fixture method on a
;; `test-suite' object to make it accessible within that suite.

(defclass fixture (test-object)
  ((name :initarg :name :initform (string (gensym "fx"))
         :accessor name)))

(defclass tmp-fixture (fixture)
  ((directory :initform #P"/tmp/" :type directory :initarg :directory :accessor dir)
   (file :initform nil :type (or null pathname string) :initarg :file :accessor file)))

(defmethod make-fixture ((kind (eql :tmp)) &rest args)
  (apply 'make-instance 'tmp-fixture args))

(defmethod make-fixture (kind &rest args)
  (apply 'make-instance kind args))

(defmacro with-fixture ((var kind &rest args) &body body)
  `(let ((,var (make-fixture ',kind ,@args)))
     ,@body))

(defmethod path ((self tmp-fixture))
  (merge-pathnames (file self) (dir self)))

;;;; Tests
(defkernel test (test-object kernel-object)
  ((bench :type (or boolean fixnum) :accessor test-bench :initform nil :initarg :bench)
   (profile :type list :accessor test-profile :initform nil :initarg :profile)
   (cover :type boolean :accessor test-cover :initform nil :initarg :cover)
   (declare :type list :accessor test-declare :initform nil :initarg :declare)
   (form :initarg :form :initform nil :accessor test-form)
   (documentaton :initarg :documentation :type string :accessor test-documentation)
   (state :initarg :state :initform 0 :accessor state)
   (persist :initarg :persist :initform nil :type boolean :accessor test-persist-p)
   (results :initarg :results :initform nil :accessor results))
  (:documentation "Test class typically made with `deftest'."))

(defmethod initialize-instance ((self test) &key form declare &allow-other-keys)
  ;; (debug! "building test" name)
  (set-funcallable-instance-function self (compile nil `(lambda () ,@(when declare `((declare ,@declare))) ,@form)))
  (call-next-method))

;; (defmethod initialize-instance :after ((self test) &key cover)
;;  (when cover (push '(optimize sb-cover:store-coverage-data) (test-declare self))))

(defmethod print-object ((self test) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A"
            (name self))))

(defmethod push-result ((self test-result) (place test))
  (push self (results place)))

(defmethod pop-result ((self test))
  (pop (results self)))

(defmethod eval-test ((self test))
  (eval `(progn ,@(test-form self))))

(defmethod funcall-test ((self test) &key declare)
  ;; ensure kernel
  (unless (functionp (kernel self))
    (trace! (setf (symbol-function (kernel self))
                  (eval `(lambda ()
                           ,(when declare `(declare ,declare))
                           ,@(test-form self))))))
  (funcall (kernel self)))

(defmethod compile-test ((self test) &key declare)
  (compile
   (kernel self)
   `(lambda ()
      ,@(when declare `((declare ,declare)))
      ,@(test-form self))))

(defmethod compile-test ((self symbol) &key declare (suite *test-suite*))
  (compile-test (test suite self) :declare declare))

(defun compile-suite (&optional (suite *test-suite*))
  (loop for test in (tests suite)
        do (compile-test test)))

(defun fail! (form &optional fmt &rest args)
  (let ((reason (and fmt (apply #'format nil fmt args))))
    (with-simple-restart (ignore-fail "Continue testing.")
      (error 'test-failed :reason reason :form form))))

(defmacro with-test-env (self &body body)
  `(catch '#.+test-tag+
     (incf (state ,self))
     (let* ((*testing* ,self)
            (*log-level* (level *test-suite*))
            (*fixtures* (test-fixtures *test-suite*))
            %test-result)
       (block %test-bail
         ,@body)
       %test-result)))

(defmethod do-test ((self test) &optional fx)
  (declare (ignorable fx))
  (with-test-env self
    (trace! "running test: ~A" *testing*)
    (flet ((%do ()
             (when (test-profile self)
               (sb-sprof:start-profiling))
             (if *compile-tests*
                 (with-compilation-unit (:override t :policy (or (and *test-suite* (test-policy *test-suite*)) *test-policy*))
                   (unwind-protect-case ()
                       (funcall (compile-test self :declare (test-declare self)))
                     (:normal (setf %test-result (make-test-result :pass (kernel self))))
                     (:abort (setf %test-result (make-test-result :fail (kernel self))))))
                 (progn
                   (funcall-test self :declare (test-declare self))
                   (setf %test-result (make-test-result :pass self))))
             (when (test-profile self)
               (sb-sprof:stop-profiling))))
      (handler-bind
          ((error 
             (lambda (c)
               (decf (state self))
               (setf %test-result (make-test-result :fail c))
               (when *catch-test-errors* (error c))
               (return-from do-test %test-result))))
        (%do)))))

(defmethod do-test ((self simple-string) &optional fixture)
  (when-let ((test (test *test-suite* self)))
    (do-test test fixture)))

(defmethod do-test ((self symbol) &optional fixture)
  (when-let ((test (test *test-suite* (symbol-name self))))
    (do-test test fixture)))

;;;; Suites
(defclass test-suite (test-object)
  ((tests :initarg :set :initform nil :type list :accessor tests
          :documentation "test-suite tests")
   (results :initarg :results :initform nil :type list :accessor results
            :documentation "test-suite results")
   (stream :initarg :stream :initform *standard-output* :type stream :accessor test-stream)
   (fixtures :initarg :fixtures :initform nil :type list :accessor test-fixtures)
   (level :initarg :level :initform *log-level* :type log-level-designator :accessor level)
   (policy :initarg :policy :initform *test-policy* :accessor test-policy)
   (logger :initarg :logger :initform *logger* :accessor test-logger))
  (:documentation "A class for collections of related `test' objects."))

(defmethod print-object ((self test-suite) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A [~d:~d:~d:~d]"
            (name self)
            (length (tests self))
            (count t (map-tests self (lambda (x) (zerop (state x)))))
            (count t (map-tests self #'test-persist-p))
            (length (results self)))))

;; (defmethod reinitialize-instance ((self test-suite) &rest initargs &key &allow-other-keys))

(deftype test-suite-designator ()
  "Either nil, a symbol, a string, or a `test-suite' object."
  '(or null symbol string test-suite keyword))

(defun test-suite (name)
  (declare (test-suite-designator name))
  (find name *test-suites* :test #'test-name=))

(defun find-fixture (name &optional (suite *test-suite*))
  (find name (test-fixtures suite) 
        :test 'string-equal
        :key 'name))

(defmethod map-tests ((self test-suite) function)
  ;; tests are stored in reverse order. run LIFO.
  (mapcar function (reverse (tests self))))

(defmethod push-test ((self test) (place test-suite))
  (push self (tests place)))

(defmethod pop-test ((self test-suite))
  (pop (tests self)))

(defmethod push-result ((self test-result) (place test-suite))
  (with-slots (results) place
    (push self results)))

(defmethod pop-result ((self test-suite))
  (pop (results self)))

(defmethod test ((self test-suite) name &key (test #'test-name=))
  (declare (type (or string symbol) name)
           (type function test))
  (find name (tests self) :test test))

(defmethod test ((self symbol) name &key (test #'test-name=))
  (test (find-suite self) name :test test))

(defmethod do-test ((self test-suite) &optional test)
  (push-result 
   (if (log:info! test)
       (do-test
           (etypecase test
             (test test)
             (string (test self test))
             (symbol (test self (symbol-name test)))))
       (do-test (pop-test self)))
   self))

(deffmt fmt-in-suite "in suite ~x:~%")
(deffmt fmt-test-result "~@[~<~%~:;~:@(~S~) ~>~]~%")
(deffmt fmt-success "~&No tests failed.~%")

;; HACK 2023-09-01: find better method of declaring failures from
;; within the body of `deftest'.
(defmethod do-suite ((self test-suite) &key stream force (error *catch-test-errors*))
  (when stream (setf (test-stream self) stream))
  (with-slots (name stream) self
    (fmt-in-suite stream name)
    (format stream "; with ~A~A tests~%"
            (if force
                ""
                (format nil "~A/"
                        (count t (tests self)
                               :key (lambda (x) (or (zerop (state x)) (test-persist-p x))))))
            (length (tests self)))
    ;; loop over each test, calling `do-test'. if locked or persistent, test
    ;; is performed. if FORCE is non-nil all tests are performed.
    (map-tests self 
               (lambda (x)
                 (when (or force (zerop (state x)) (test-persist-p x))
                   (let ((res (do-test x)))
                     (push-result res self)
                     (fmt-test-result stream res)))))
    ;; compare locked vs expected
    (let ((locked (remove-if #'null (map-tests self (lambda (x) (when (zerop (state x)) x)))))
          (fails
            ;; collect if locked test not expected
            (loop for r in (results self)
                  unless (test-pass-p r)
                  collect r)))
      (if (null locked)
          (fmt-success stream)
          (progn
            (format stream "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                    (length locked)
                    (length (tests self))
                    locked)
            (unless (null fails)
              (format stream "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                      (length fails)
                      fails))))
      ;; close stream
      (finish-output stream)
      ;; return values (PASS? LOCKED)
      (values (or (not fails) 
                  (when error (error 'test-failed :name (name self) :form fails :reason "Failure in test suite")))
              locked))))

(defmethod do-suite ((self string) &key stream)
  (do-suite (ensure-suite self) :stream stream))

(defmethod do-suite ((self symbol) &key stream)
  (do-suite (ensure-suite self) :stream stream))

(defmethod do-suite ((self null) &key stream)
  (do-suite *test-suite* :stream stream))

;;; Threading
(defkernel test-worker (task-worker) ())
(defclass test-pool (task-pool) ())
;; test plan
