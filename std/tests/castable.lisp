;;; castable.lisp --- CASTABLE Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)
;;; Castable
(deftest castable-solo ()
  (is (typep (make-castable) 'castable))
  (is (null (clrchash (make-castable))))
  (is (eql nil (getchash nil (make-castable))))
  (is (eql nil (getchash t (make-castable))))
  (is (eql t (getchash nil (make-castable) t)))
  (is (eql t (setf (getchash t (make-castable)) t)))
  (is (null (remchash t (make-castable))))
  (let ((map (make-castable))) ;; basic
    (is (setf (getchash t map) t))
    (is (getchash t map))
    (is (remchash t map))
    (is (null (getchash t map)))
    (is (null (remchash t map))))
  (let ((map (make-castable))) ;; rizzlin and sizzlin
    (is (null (dotimes (i 100) (setf (getchash i map) i))))
    (is (loop for i from 0 below 100 always (= i (getchash i map))))
    (is (= 100 (castable-count map)))
    (is (null (clrchash map)))
    (is (= 0 (castable-count map)))
    (is (null (getchash 0 map))))
  (let ((map (make-castable :test 'eq)) ;;eq
        (key (make-string 1 :initial-element #\a)))
    (is (setf (getchash key map) t))
    (is (getchash key map))
    (is (null (getchash (make-string 1 :initial-element #\a) map))))
  (let ((map (make-castable :test 'eql))) ;;eql
    (is (setf (getchash 0 map) t))
    (is (getchash 0 map))
    (is (null (getchash 0.0 map))))
  (let ((map (make-castable :test 'equal))) ;;equal
    (is (setf (getchash "a" map) t))
    (is (null (getchash "A" map))))
  (let ((map (make-castable :test 'equalp))) ;;equalp
    (is (setf (getchash #\a map) t))
    (is (getchash #\A map))))

(deftest castable-multi (:skip t)
  (let ((tries 40000)
        (threads 4))
    (let ((table (make-castable)))
      (with-threads (_ threads)
        (loop repeat tries do (setf (getchash t table) t)))
      (is (eql t (getchash t table)))
      (is (= 1 (castable-count table))))
    (let ((table (make-castable))
          (/thread (floor (/ tries threads))))
      (finish-threads
       (with-threads (idx threads)
         (loop for i from (* idx /thread) below (* (1+ idx) /thread)
               do (setf (getchash i table) i))))
      (print (castable-count table))
      ;; (is (= tries (castable-count table)))
      (is (loop for i from 0 below tries
                do (print (getchash i table))
                always (equal i (getchash i table)))))
    ;; Concurrent set on same fields
    (let ((table (make-castable)))
      (finish
       (finish-threads
        (with-threads (idx threads)
          (loop for i from 0 below tries
                do (setf (getchash i table) i)))))
      (is = tries (castable-count table))
      (is eql T (loop for i from 0 below tries
                      always (eql i (getchash i table)))))
    ;; Concurrent set on randomised fields
    (let ((table (make-castable)))
      (flet ((random-index (idx i)
               (floor (* tries (/ (sxhash (+ (* idx tries) i)) most-positive-fixnum)))))
        (finish
         (finish-threads
          (with-threads (idx threads)
            (loop for i from 0 below tries
                  for j = (random-index idx i)
                  do (setf (getchash j table) j)))))
        (is <= tries (castable-count table))))
    ;; Concurrent set & remove
    (let ((table (make-castable)))
      (finish
       (finish-threads
        (with-threads (idx (/ threads 2))
          (loop for i from idx below tries by threads
                do (setf (getchash i table) i)))
        (with-threads (idx (/ threads 2))
          (loop for i from idx below tries by threads
                do (loop until (remchash i table))))))
      (is = 0 (castable-count table)))))
