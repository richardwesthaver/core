(defpackage :log/tests
  (:use :cl :std :rt :log))

(in-package :log/tests)

(eval-always
  (defclass logger-fixture (logger fixture) ())
  (defmethod make-fixture ((kind (eql :logger)) &rest args)
    (apply 'make-instance 'logger-fixture args)))


(defsuite :log :level :trace)
  
(in-suite :log)

(deftest simple-log-message ()
  "Test a simple LOG-MESSAGE"
  (with-fixture (fx :logger)
    (istype 'string (format-message nil (make-instance 'simple-log-message :content "hi" :tags '(:test))))
    (istype 'thread (start fx))
    (istype 'string (format-message nil (log-message :error nil "test")))))

(deftest simple-logger (:use (fx :logger))
  "Test a simple LOGGER."
  (istype 'logger-fixture fx)
  (log-message :info '(:foo :bar) "this is a test"))

;; TODO 2024-10-29: fix file loggers
(deftest file-logger ()
  "Test a file-backed LOGGER."
  (with-fixture (tmp :tmp :file (tmpize-pathname "test.log"))
    (with-fixture (fx :logger)
    (unwind-protect
         (with-logger fx
           (let ((tmpfile (path tmp)))
             ;; (is *logger*)
             (log-pipe (make-instance 'file-sink :file tmpfile))
             (unless (started-p fx)
               (start fx))
             (log-message :info '(:file :log) "test")
             (sleep 1)
             (is> 0 (file-size tmpfile))))
      (delete-file (path tmp))))))

(deftest rotating-file-logger ()
  (with-fixture (tmp :tmp :file (tmpize-pathname "log"))
    (with-fixture (fx :logger)
      (with-logger fx
        (let ((tmpfile (path tmp)))
          (is *logger*)
          (setf (pipe *logger*) (make-pipe))
          (log-pipe (make-instance 'rotating-file-sink :path tmpfile))
          (is (probe-file (file (aref (aref (pipe *logger*) 0) 0))))
          (log-message :info nil "rotating log test")
          (log::rotate-file-sink (aref (aref (pipe *logger*) 0) 0))
          (log-message :info nil "rotating test2")
          (is> 0 (file-size (print (file (aref (aref (pipe *logger*) 0) 0)))))
          (is (delete-file (file (aref (aref (pipe *logger*) 0) 0)))))))))

(deftest simple-log ()
  "Test logging features"
  (is (debug! "test" *log-level*))
  (is (info! "test"))
  (is (trace! "test"))
  (is (error! "test"))
  (is (fatal! "test"))
  (is (warn! "test")))

(deftest stream ()
  (let ((str (random-bytes 1024))
        (lock (make-mutex))
        (*tmp* (tmpize-pathname "/tmp/log-stream")))
    (with-log-stream (st *tmp* lock)
      (write-sequence str st))
    (is (= (length str) (file-size *tmp*)))
    (delete-file *tmp*)))

(deftest fast-stream ()
  (let ((str (random-bytes 1024))
        (lock (make-mutex))
        (*tmp* (tmpize-pathname "/tmp/fast-log-stream")))
    (with-fast-log-stream (st *tmp* lock)
      (io/fast:fast-write-sequence str st))
    (is (= (length str) (file-size *tmp*)))
    (delete-file *tmp*)))

(deftest stop-logger ()
  (with-fixture (fx :logger)
    (stop fx)))

(defmethod db:make-db ((engine (eql :faux-log)) &key)
  (make-array '(4 100)))

(defclass faux-db-sink (db-sink) ())

(defvar *faux-log* (make-instance 'database-logger :db (db:make-db :faux-log)))

(defun faux-level (int)
  (coerce
   (loop for i below 100
         collect (row-major-aref (db:db *faux-log*) (+ i int)))
   'vector))

(defmethod schema:column ((self faux-db-sink) (col integer)) (faux-level (* 100 col)))

(deftest db-logger ())

