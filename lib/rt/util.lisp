;;; rt/util.lisp --- Test Utilities

;;

;;; Commentary:

;; some of these are taken from the TEST-UTIL package of SBCL
;; (tests/test-util.lisp)

;;; Code:
(in-package :rt)

;;; tmp
(defmacro with-tmp-directory ((&optional (name (string (gensym "tmp")))
                                         (defaults *default-tmp-directory*))
                              &body body)
  `(let ((*tmp* (directory-path (merge-pathnames ,name ,defaults))))
     (ensure-directories-exist *tmp*)
     (unwind-protect (progn ,@body)
       (sb-ext:delete-directory *tmp* :recursive t))))

(defmacro with-tmp-file ((stream-var &key (name (string (gensym "tmp")))
                                          type
                                          (directory *default-tmp-directory*)
                                          (direction :output)
                                          (if-exists :supersede)
                                          (element-type ''character))
                         &body body)
  `(let ((*tmp* (make-pathname :name ,name :type ,type :directory ,(namestring directory))))
     (with-open-file (,stream-var *tmp* :direction ,direction :element-type ,element-type
                                  :if-exists ,if-exists)
       (unwind-protect (progn ,@body)
         (delete-file *tmp*)))))

;;; random
(defvar *simple-charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun random-ref (vec)
  (aref vec (random (length vec))))

(defun random-char ()
  (random-ref *simple-charset*))

(defun random-chars (dim)
  (let ((r (make-array dim :element-type 'character)))
    (dotimes (i (array-total-size r) r)
      (setf (row-major-aref r i) (random-char)))))

(defun random-byte () (random 255))

(defun random-bytes (dim)
  (let ((r (make-array dim :element-type 'octet)))
    (dotimes (i (array-total-size r) r)
      (setf (row-major-aref r i) (random-byte)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-test (&rest slots)
    (apply #'make-instance 'test slots))
  (defun make-suite (&rest slots)
    (apply #'make-instance 'test-suite slots)))

;; TODO 2023-09-04: optimize
;;(declaim (inline do-tests))
(defun do-tests (&optional (suite *test-suite*) force (output *standard-output*))
  (if (pathnamep output)
      (with-open-file (stream output :direction :output)
        (do-suite (ensure-suite suite) :stream stream :force force))
      (do-suite (ensure-suite suite) :stream output :force force)))

(defvar *test-output-mutex* (sb-thread:make-mutex :name "tests-output"))

;; TODO
(defun do-tests-concurrently (&optional (suite *test-suite*) force (output *standard-output*))
  (declare (ignore suite force))
  (sb-thread:with-mutex (*test-output-mutex*)
    (let ((stream (make-synonym-stream output)))
      (let ((*standard-output* stream)
            (*error-output* stream))
        (nyi!)))))

(defun reset-tests ()
  (setq *testing* nil
        *test-suite* nil
        *fx* nil
        *test-suite-list* nil
        *test-input* nil
        *test-output* nil))

;; this assumes that *test-suite* is re-initialized correctly to the
;; correct test-suite object.
(defun continue-testing ()
  (if-let ((test *testing*))
    (throw '%in-test test)
    (do-suite *test-suite*)))

;; NOTE 2023-09-01: `pushnew' does not return an indication of whether
;; place is changed - it returns place. This is functionally sound but
;; means that if we want to do something else in the event that place
;; is unchanged, we run into some friction,
;; https://stackoverflow.com/questions/56228832/adapting-common-lisp-pushnew-to-return-success-failure
(defun spush (item lst &key (test #'equal))
  "Substituting `push'"
  (declare (type function test))
  (cond
    ((null lst) (push item lst))
    ((list lst)
     (if-let ((found (member item lst
                             :test test)))
       (progn
         (rplaca found item)
         lst)
       (push item lst)))
    #|(or nil '(t (cons item lst)))|#))

;; FIX 2023-08-31: spush, replace with `add-test' method.
;; (declaim (inline normalize-test-name))
(defun normalize-test-name (a)
  "Return the normalized `test-suite-designator' of A."
  (etypecase a
    (string (string-upcase a))
    (symbol (symbol-name a))
    (test-object (normalize-test-name (name a)))
    (t (format nil "~A" a))))

(defun test-name= (a b)
  "Return t if A and B are similar `test-suite-designator's."
  (let ((a (normalize-test-name a))
        (b (normalize-test-name b)))
    (string= a b)))

(declaim (inline assert-suite ensure-suite))
(defun ensure-suite (name)
  (if-let ((ok (member name *test-suite-list* :test #'test-name=)))
    (car ok)
    (when (or (eq name t) (null name)) (make-suite :name *default-test-suite-name*))))

(defun check-suite-designator (suite) (check-type suite test-suite-designator))

(defun assert-suite (name)
  (check-suite-designator name)
  (assert (ensure-suite name)))
