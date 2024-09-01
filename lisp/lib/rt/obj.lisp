;;; obj.lisp --- Test Objects

;; 

;;; Code:
(in-package :rt)

;;; Result
(deftype result-tag ()
  '(or (member :pass :fail :skip) null))

(declaim (inline %make-test-result))
(defstruct (test-result (:constructor %make-test-result)
                        (:conc-name  tr-))
  (tag nil :type result-tag :read-only t)
  (form nil :type form))

(defmethod print-object ((self test-result) stream)
  (print-unreadable-object (self stream :identity t)
    (format stream "~A ~A" (tr-tag self) (tr-form self))))

(defun make-test-result (tag &optional form)
  (%make-test-result :tag tag :form form))

(defmethod test-pass-p ((res test-result))
  (when (eq :pass (tr-tag res)) t))

(defmethod test-fail-p ((res test-result))
  (when (eq :fail (tr-tag res)) t))

(defmethod test-skip-p ((res test-result))
  (when (eq :skip (tr-tag res)) t))

(defmethod print-object ((self test-result) stream)
  (print-unreadable-object (self stream)
    (format stream "~A ~A"
            (tr-tag self)
            (tr-form self))))

;;; Test Object
(defclass test-object ()
  ((name :initarg :name :initform (required-argument) :type string :accessor test-name)
   #+nil (cached :initarg :cache :allocation :class :accessor test-cached-p :type boolean))
  (:documentation "Super class for all test-related objects."))

(defmethod print-object ((self test-object) stream)
  "test"
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A"
            (test-name self))))

;;;; Tests
(defclass test (test-object)
  ((fn :type symbol :accessor test-fn)
   (bench :type (or boolean fixnum) :accessor test-bench :initform nil :initarg :bench)
   (profile :type list :accessor test-profile :initform nil :initarg :profile)
   (args :type list :accessor test-args :initform nil :initarg :args)
   (declare :type list :accessor test-declare :initform nil :initarg :declare)
   (form :initarg :form :initform nil :accessor test-form)
   (doc :initarg :doc :type string :accessor test-doc)
   (lock :initarg :lock :type boolean :accessor test-lock-p)
   (persist :initarg :persist :initform nil :type boolean :accessor test-persist-p)
   (results :initarg :results :type (array test-result) :accessor test-results))
  (:documentation "Test class typically made with `deftest'."))

(defmethod initialize-instance ((self test) &key name)
  ;; (debug! "building test" name)
  (setf (test-fn self)
        (make-symbol
         (format nil "~A~A"
                 name
                 (gensym *test-suffix*))))
  (setf (test-lock-p self) t)
  ;; TODO 2023-09-21: we should count how many checks are in the :form
  ;; slot and infer the array dimensions.
  (setf (test-results self) (make-array 0 :element-type 'test-result))
  (call-next-method))

(defmethod print-object ((self test) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A :fn ~A"
            (test-name self)
            (test-fn self))))

(defmethod push-result ((self test-result) (place test))
  (with-slots (results) place
    (push self results)))

(defmethod pop-result ((self test))
  (pop (test-results self)))

(defmethod eval-test ((self test))
  (eval `(progn ,@(test-form self))))

(defmethod funcall-test ((self test) &key declare)
  (unless (functionp (test-fn self))
    (trace! (setf (symbol-function (test-fn self))
                  (eval `(lambda ()
                           ,(when declare `(declare ,declare))
                           ,@(test-form self))))))
  (funcall (test-fn self)))

(defmethod compile-test ((self test) &key declare &allow-other-keys)
  (with-compilation-unit (:policy '(optimize debug))
    (compile
     (test-fn self)
     `(lambda ()
        ,(when declare `(declare ,declare))
        ,@(test-form self)))))

(defun fail! (form &optional fmt &rest args)
  (let ((reason (and fmt (apply #'format nil fmt args))))
    (with-simple-restart (ignore-fail "Continue testing.")
      (error 'test-failed :reason reason :form form))))

(defmacro with-test-env (self &body body)
  `(catch '%in-test
     (setf (test-lock-p ,self) t)
     (let* ((*testing* ,self)
            (%test-bail nil)
            %test-result)
       (block %test-bail
         ,@body
         (setf (test-lock-p ,self) %test-bail))
       %test-result)))

(defmethod do-test ((self test) &optional fx)
  (declare (ignorable fx))
  (with-test-env self
    (trace! "running test: " *testing*)
    (flet ((%do ()
             (if-let ((opt *compile-tests*))
               ;; RESEARCH 2023-08-31: with-compilation-unit?
               (progn
                 (if (eq opt t)
                     (setq opt *test-opts*)
                     (setq opt (push *test-opts* opt)))
                 ;; TODO 2023-09-21: handle failures here
                 (funcall (compile-test self :declare opt))
                 (setf %test-result (make-test-result :pass (test-fn self))))
               (progn
                 (funcall-test self :declare '(optimize (debug 3) (safety 0)))
                 (setf %test-result (make-test-result :pass (test-name self)))))))
      (if *catch-test-errors*
          (handler-bind
              ((error 
                 (lambda (c)
                   (setf %test-bail t)
                   (setf %test-result (make-test-result :fail c))
                   (return-from %test-bail %test-result))))
            (%do))
          (%do)))))

(defmethod do-test ((self simple-string) &optional fixture)
  (when-let ((test (find-test *test-suite* self)))
    (do-test test fixture)))

(defmethod do-test ((self symbol) &optional fixture)
  (when-let ((test (find-test *test-suite* (symbol-name self))))
    (do-test test fixture)))

;;;; Suites
(defclass test-suite (test-object)
  ((tests :initarg :set :initform nil :type list :accessor tests
          :documentation "test-suite tests")
   (results :initarg :results :initform nil :type list :accessor test-results
            :documentation "test-suite results")
   (stream :initarg :stream :initform *standard-output* :type stream :accessor test-stream)
   (fixtures :initarg :fixtures :initform nil :type list :accessor test-fixtures))
  (:documentation "A class for collections of related `test' objects."))

(defmethod print-object ((self test-suite) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A [~d:~d:~d:~d]"
            (test-name self)
            (length (tests self))
            (count t (map-tests self #'test-lock-p))
            (count t (map-tests self #'test-persist-p))
            (length (test-results self)))))

;; (defmethod reinitialize-instance ((self test-suite) &rest initargs &key &allow-other-keys))

(deftype test-suite-designator ()
  "Either nil, a symbol, a string, or a `test-suite' object."
  '(or null symbol string test-suite keyword))

(defun find-suite (name)
  (declare (test-suite-designator name))
  (find name *test-suite-list* :test #'test-name=))

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
  (pop (test-results self)))

(defmethod find-test ((self test-suite) name &key (test #'test-name=))
  (declare (type (or string symbol) name)
           (type function test))
  (find name (tests self) :test test))

(defmethod do-test ((self test-suite) &optional test)
  (push-result 
   (if test
       (do-test
           (etypecase test
             (test test)
             (string (find-test self test))
             (symbol (find-test self (symbol-name test)))))
       (do-test (pop-test self)))
   self))

;; HACK 2023-09-01: find better method of declaring failures from
;; within the body of `deftest'.
(defmethod do-suite ((self test-suite) &key stream force)
  (when stream (setf (test-stream self) stream))
  (with-slots (name stream) self
    (format stream "in suite ~x:~%"
            name)
    (format stream "; with ~A~A tests~%"
            (if force
                ""
                (format nil "~A/"
                        (count t (tests self)
                               :key (lambda (x) (or (test-lock-p x) (test-persist-p x))))))
            (length (tests self)))
    ;; loop over each test, calling `do-test'. if locked or
    ;; persistent, test is performed. if FORCE is non-nil all tests
    ;; are performed.
    (map-tests self 
               (lambda (x)
                 (when (or force (test-lock-p x) (test-persist-p x))
                   (let ((res (do-test x)))
                     (push-result res self)
                     (format stream "~@[~<~%~:;~:@(~S~) ~>~]~%" res)))))
    ;; compare locked vs expected
    (let ((locked (remove-if #'null (map-tests self (lambda (x) (when (test-lock-p x) x)))))
          (fails
            ;; collect if locked test not expected
            (loop for r in (test-results self)
                  unless (test-pass-p r)
                  collect r)))
      (if (null locked)
          (format stream "~&No tests failed.~%")
          (progn
            ;;  RESEARCH 2023-09-04: print fails ??
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
      (values (not fails) locked))))

(defmethod do-suite ((self string) &key stream)
  (do-suite (ensure-suite self) :stream stream))

(defmethod do-suite ((self symbol) &key stream)
  (do-suite (ensure-suite self) :stream stream))

(defmethod do-suite ((self null) &key stream)
  (do-suite *test-suite* :stream stream))

;;; Fixtures
;; Our fixtures are objects which can be inherited to build different fixture
;; classes. Fixtures inherit from TEST-OBJECT and have a NAME which usually
;; indicates the key used to initialize this object with MAKE-INSTANCE.

;; You can use fixtures inside a test or use the push-fixture method on a
;; `test-suite' object to make it accessible within that suite.

(defclass fixture (test-object) ())

(defclass tmp-fixture (fixture)
  ((directory :initform #P"/tmp/" :type directory :initarg :directory)
   (file :initform nil :type (or null pathname string) :initarg :file))
  (:default-initargs
   :name :tmp))

(defmethod make-fixture ((kind (eql :tmp)) &rest args)
  (apply 'make-instance 'tmp-fixture args))

(defmacro with-fixture ((var (kind &rest args)) &body body)
  `(let ((,var (make-fixture ,kind ,@args)))
     ,@body))
