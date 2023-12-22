(defpackage :obj/tests
  (:use :cl :std :rt :obj))

(in-package :obj/tests)

(defsuite :obj)
(in-suite :obj)

(deftest rainbow ())

(deftest tables ())

(deftest ids ()
  (is (= (reset-id t) (reset-id '(1 2 3))))
  (is (not (equalp (make-id) (make-id)))))

(deftest def-iter ())

(deftest def-seq ())

;; TODO 2023-12-17: 
;; (deftest uris ()
;;   "Tests for different types of URIs. Attempts to conform with RFCs and test suites."
;;   (uri-host (parse-uri-string-rfc3986 "https://localhost"))
;; )

(deftest castable-solo ()
  (is (typep (make-castable) 'castable))
  (is (null (cclrhash (make-castable))))
  (is (eql nil (cgethash nil (make-castable))))
  (is (eql nil (cgethash t (make-castable))))
  (is (eql t (cgethash nil (make-castable) t)))
  (is (eql t (setf (cgethash t (make-castable)) t)))
  (is (null (cremhash t (make-castable))))
  (let ((map (make-castable))) ;; basic
    (is (setf (cgethash t map) t))
    (is (cgethash t map))
    (is (cremhash t map))
    (is (null (cgethash t map)))
    (is (null (cremhash t map))))
  (let ((map (make-castable))) ;; rizzlin and sizzlin
    (is (null (dotimes (i 100) (setf (cgethash i map) i))))
    (is (loop for i from 0 below 100 always (= i (cgethash i map))))
    (is (= 100 (castable-count map)))
    (is (null (cclrhash map)))
    (is (= 0 (castable-count map)))
    (is (null (cgethash 0 map))))
  (let ((map (make-castable :test 'eq)) ;;eq
        (key (make-string 1 :initial-element #\a)))
    (is (setf (cgethash key map) t))
    (is (cgethash key map))
    (is (null (cgethash (make-string 1 :initial-element #\a) map))))
  (let ((map (make-castable :test 'eq))) ;;eql
    (is (setf (cgethash 0 map) t))
    (is (cgethash 0 map))
    (is (null (cgethash 0.0 map))))
  (let ((map (make-castable :test 'equal))) ;;equal
    (is (setf (cgethash "a" map) t))
    (is (null (cgethash "A" map))))
  (let ((map (make-castable :test 'equalp))) ;;equalp
    (is (setf (cgethash #\a map) t))
    (is (cgethash #\A map))))

(deftest castable-multi (:disabled t)
  (let ((tries 4000)
        (threads 4))
    (let ((table (make-castable)))
      (with-threads (_ threads)
        (loop repeat tries do (setf (cgethash t table) t)))
      (is (eql t (cgethash t table)))
      (is (= 1 (castable-count table))))
    (let ((table (make-castable))
          (/thread (floor (/ tries threads))))
      (finish-threads
       (with-threads (idx threads)
         (loop for i from (* idx /thread) below (* (1+ idx) /thread)
               do (setf (cgethash i table) i))))
      (print (castable-count table))
      ;; (is (= tries (castable-count table)))
      (is (loop for i from 0 below tries
                do (print (cgethash i table))
                always (equal i (cgethash i table)))))
    ;; Concurrent set on same fields
    (let ((table (make-castable)))
      (finish
       (finish-threads
        (with-threads (idx threads)
          (loop for i from 0 below tries
                do (setf (cgethash i table) i)))))
      (is = tries (castable-count table))
      (is eql T (loop for i from 0 below tries
                      always (eql i (cgethash i table)))))
    ;; Concurrent set on randomised fields
    (let ((table (make-castable)))
      (flet ((random-index (idx i)
               (floor (* tries (/ (sxhash (+ (* idx tries) i)) most-positive-fixnum)))))
        (finish
         (finish-threads
          (with-threads (idx threads)
            (loop for i from 0 below tries
                  for j = (random-index idx i)
                  do (setf (cgethash j table) j)))))
        (is <= tries (castable-count table))))
    ;; Concurrent set & remove
    (let ((table (make-castable)))
      (finish
       (finish-threads
        (with-threads (idx (/ threads 2))
          (loop for i from idx below tries by threads
                do (setf (cgethash i table) i)))
        (with-threads (idx (/ threads 2))
          (loop for i from idx below tries by threads
                do (loop until (cremhash i table))))))
      (is = 0 (castable-count table)))))
