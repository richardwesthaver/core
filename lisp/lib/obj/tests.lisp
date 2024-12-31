(defpackage :obj/tests
  (:use :cl :std :rt 
   :obj :uuid :url :std/macs
   :dynamic :fast :sealed :stealth :stored :store))

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

(deftest ids ()
  (is (= (reset-id t) (reset-id '(1 2 3))))
  (is (not (equalp (make-id nil) (make-id nil)))))

(deftest uuids ()
  (macrolet ((is-uuid (obj) `(is (typep ,obj 'uuid))))
    (is-uuid (make-v1-uuid))
    (is-uuid (make-v4-uuid))))

(deftest def-iter ())

(deftest def-seq ())

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

(deftest generic-tree ()
  (let ((tree (make-binary-node
              0 
              (make-binary-node 1 (make-node 0) (make-node 1))
              (make-binary-node 2 (make-node 2) (make-node 3)))))
    (is (typep tree 'binary-node))))

(deftest bro-tree ()
  (is (sb-brothertree::make-binary-node 0 nil nil)))

(deftest rb-tree ())

(deftest avl-tree ()
  (is (make-avl-node 0 0 nil nil)))

(deftest btree ()
  (is (make-instance 'btree)))

(deftest basic-graph ()
  "Test basic graph functionality."
  (let ((g1 (make-instance 'graph:graph)))
    (is (typep g1 'graph:graph))
    (graph:add-node g1 :foo)
    (graph:add-node g1 :bar)
    (graph:add-edge g1 '(:foo :bar))
    ;; graph is undirected, so this is no-op
    (graph:add-edge g1 '(:bar :foo))
    ;; and only 1 edge exists
    (is (= 1 (length (hash-table-keys (graph:edges g1)))))
    (let ((g2 (make-instance 'graph:directed-graph)))
      (is (typep g2 'graph:directed-graph))
      (graph:add-node g2 :baz)
      (graph:add-node g2 :buz)
      (graph:add-edge g2 '(:baz :buz))
      ;; graph is directed, so this is a unique edge
      (graph:add-edge g2 '(:buz :baz))
      ;; 2 edges exist
      (is (= 2 (length (hash-table-keys (graph:edges g2)))))
      ;; (graph:add-node g1 g2)
      ;; (is (graph::has-node-p g1 g2))
      ;; (graph::delete-node g1 g2)
      ;; (is (not (graph::has-node-p g1 g2)))
      )))

;; TODO 2023-12-17: 
(deftest uris ()
  "Tests for different types of URIs. Attempts to conform with RFCs and test suites."
  (let ((local #.(parse-uri "https://localhost/stash/index.json"))
        (local2 (parse-uri "https://localhost/stash/index.json"))
        (ftp (parse-uri "ftp://ftp.is.co.za/rfc/rfc1808.txt")))
    (is (equal "localhost" (uri-host local)))
    (is (eql :ftp (uri-scheme ftp)))
    (is (= (obj/uri::uri-hash local) (obj/uri::uri-hash local2)))
    (is (equal "foo%25bar" (uri-path (parse-uri "foo%25bar"))))
    (is (equal "/test/foo%25bar.lisp"
	       (uri-to-string (string-to-uri "/test/foo%25bar.lisp"))))
    (is (equal
         "/test/foo%25bar.lisp"
	 (render-uri (parse-uri "/test/foo%25bar.lisp") nil)))
    (is (equal "http://franz.com/foo?val=a%2b%3d%26b+is+c"
               (render-uri (parse-uri "http://franz.com/foo?val=a%2b%3d%26b+is+c") nil)))

    (dolist (xx ;; (list user-info ipaddr port)
	     '((nil "192.132.95.22" nil)
	       (nil "192.132.95.22" 81)
	       ("layer" "192.132.95.22" nil)
	       ("layer" "192.132.95.22" 81)
		("layer:pass" "192.132.95.22" nil)
		("layer:pass" "192.132.95.22" 81)
		(nil "fe80::230:48ff:feb9:bbea" nil)
		(nil "fe80::230:48ff:feb9:bbea" 81)
		(nil "2001:470:1f05:548:230:48ff:feb9:bbea" nil)
		(nil "2001:470:1f05:548:230:48ff:feb9:bbea" 81)
		(nil "::1" nil)
		(nil "::1" 81)))
      (destructuring-bind (user-info host port) xx
        (let* ((h (if (and (stringp host) (find #\: host))
                      (format nil "[~a]" host)
                      host))
	     (s (format nil "https://~@[~a@~]~a~a/foo.html"
			user-info h (or (when port (format nil ":~d" port)) "")))
	     (u (parse-uri s)))
	  (is (string= s (princ-to-string u)))
	  (is (string= host (uri-host u)))
	  (when user-info
	    (is (string= user-info (uri-userinfo u))))
	  (is (equal port (uri-port u))))))))

(deftest url ()
  (is (equal (url-encode "/fooあ") (url-encode (url-decode "%2Ffoo%E3%81%82")))))

;;; Query
(defclass bogus-data-source (data-source) ((db :initform nil :initarg :db)))

(defvar *basic-query* "SELECT * FROM employee WHERE state = 'CT'")

(deftest query-basic ()
  "Test the simple query `SELECT * FROM employee WHERE state = 'CT'` by manually
building a query-plan."
  (make-query *basic-query*))

;;; Meta

;;;; Fast
(defgeneric %test-+ (a b)
  (:generic-function-class fast-generic-function))

;; can't be in same file :(
;; (with-compilation-unit ()
;;   (defmethod %test-+ ((a number) (b number))  
;;     (+ a b))
;;   (seal-domain #'test-+ '(number number)))

;;;; Dynamic
(defclass dyno1 (id)
  ((id :dynamic t :accessor id))
  (:metaclass dynamic-class))

(deftest dynamic-class ()
  (let ((obj (make-instance 'dyno1 :id 1)))
    (slot-dvar* obj 'id)
    (slot-dlet (((obj 'id) 0))
      (iszero (id:id obj)))
    (is> 0 (id:id obj))))

;;;; Stealth
(defclass stealth-target () ())

(deftest stealth-mixin ()
  (add-mixin 'id 'stealth-target)
  (issubclass 'id 'stealth-target)
  (define-stealth-mixin stealth-mixer (secret-object) stealth-target
    ())
  (issubclass 'secret-object 'stealth-mixer)
  (issubclass 'secret-object 'stealth-target)
  (issubclass 'stealth-mixer 'stealth-target))

;;;; Filtered
(defmethod fac ((n number))
  (* n (fac (- n 1))))

(defmethod fac ((n (eql 0)))
  1)

(deftest filtered-function ()
  (is= 362880 (fac 10)))

;;;; Typed
;; TODO

;;;; Stored
(stored:defsclass person ()
  ((name :accessor name :initarg :name :index t)
   (age :accessor age :initarg :age)
   (father :accessor father :initarg :father)
   (school :accessor school :initarg :school)))

(stored:defsclass school ()
  ((name :accessor name :initarg :name :indexed t)))

(deftest stored ()
  (with-transaction ()
    (mapcar #'(lambda (initargs) (apply #'make-instance 'school initargs))
            '((:name "West Side")
              (:name "Fitch")
              (:name "Cutler")))
    (mapcar #'(lambda (initargs) (apply #'make-instance 'person initargs))
            `((:name "Bob" :age 40 :father nil 
                     :school ,(get-instance-by-value 'school 'name "Cutler"))))
    (mapcar #'(lambda (initargs) (apply #'make-instance 'person initargs))
            `((:name "Fred" :age 12 :father nil 
                     :school ,(get-instance-by-value 'school 'name "West Side"))
              (:name "Sally" :age 30 :father ,(get-instance-by-value 'person 'name "Bob")
                     :school ,(get-instance-by-value 'school 'name "Fitch"))
              (:name "George" :age 18 :father ,(get-instance-by-value 'person 'name "Bob")
                     :school ,(get-instance-by-value 'school 'name "Cutler"))))))
;;;; Store
