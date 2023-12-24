(defpackage :obj/tests
  (:use :cl :std :rt :obj))

(in-package :obj/tests)

(defsuite :obj)
(in-suite :obj)

(defun eps= (a b &optional (eps 1e-10))
  (<= (abs (- a b)) eps))

(defun rgb= (rgb1 rgb2 &optional (eps 1e-10))
  "Compare RGB colors for (numerical) equality."
  (let ((r1 (rgb-red rgb1))
        (g1 (rgb-green rgb1))
        (b1 (rgb-blue rgb1))
        (r2 (rgb-red rgb2))
        (g2 (rgb-green rgb2))
        (b2 (rgb-blue rgb2)))
    (and (eps= r1 r2 eps)
         (eps= g1 g2 eps)
         (eps= b1 b2 eps))))

(defun random-rgb ()
  (rgb (random 1d0) (random 1d0) (random 1d0)))

(defun random-hsv ()
  (hsv (random 360d0) (random 1d0) (random 1d0)))

(deftest colors ()
  (loop repeat 100 do
    (let ((rgb (random-rgb))
          (hsv (random-hsv)))
      (is (typep (as-hsv rgb) 'hsv))
      (is (typep (as-rgb hsv) 'rgb))))
  (let ((rgb (rgb 0.070 0.203 0.337)))
    (is (equal "#123456" (print-hex-rgb rgb)))
    (is (equal "123456" (print-hex-rgb rgb :hash nil)))
    (is (equal "#135" (print-hex-rgb rgb :short t)))
    (is (equal "135" (print-hex-rgb rgb :short t :hash nil)))
    (is (equal "12345678" (print-hex-rgb rgb :alpha 0.47)))
    (is (equal "#1357" (print-hex-rgb rgb :alpha 0.47 :short t)))
    (is (equal "1357" (print-hex-rgb rgb :alpha 0.47 :hash nil :short t)))
    (is (rgb= rgb (parse-hex-rgb "#123456") 0.01))
    (is (rgb= rgb (parse-hex-rgb "123456") 0.01))
    (is (rgb= rgb (parse-hex-rgb "#135") 0.01))
    (is (rgb= rgb (parse-hex-rgb "135") 0.01))
    (flet ((aux (l1 l2)
             (and (rgb= (car l1) (car l2) 0.01)
                  (eps= (cadr l1) (cadr l2) 0.01))))
      (is (aux (list rgb 0.47) (multiple-value-list (parse-hex-rgb "#12345678"))))
      (is (aux (list rgb 0.47) (multiple-value-list (parse-hex-rgb "12345678"))))
      (is (aux (list rgb 0.47) (multiple-value-list (parse-hex-rgb "#1357"))))
      (is (aux (list rgb 0.47) (multiple-value-list (parse-hex-rgb "1357")))))
    (is (equal "#123456" (with-output-to-string (*standard-output*)
                           (print-hex-rgb rgb :destination t))))
    (is (rgb= rgb (parse-hex-rgb "foo#123456zzz" :start 3 :end 10) 0.001))))

(defun random-csv-file (&optional (name (symbol-name (gensym))) (n 1000))
  (let ((path (merge-pathnames (format nil "~a.csv" name) "/tmp/")))
    (with-open-file (f path :direction :output)
      (dotimes (i n) (format f "~a,test~a,~x,~%" i (+ n i) (random 8d0))))
    path))

(deftest tables ()
  (let ((csv (random-csv-file)))
    (is (typep (read-csv csv) 'table))))

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
  (let ((tries 40000)
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

(deftest ring ())

(deftest generic-tree ()
  (let ((dag 
          (make-binary-node 0 
                            (make-binary-node 1 (make-node 0) (make-node 1))
                            (make-binary-node 2 (make-node 2) (make-node 3)))))))
(deftest bro-tree ())

(deftest rb-tree ())

(deftest avl-tree ())

(deftest graph ())
