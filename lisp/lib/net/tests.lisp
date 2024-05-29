(defpackage :net/tests
  (:use :rt :std :cl :net :sb-concurrency :sb-thread :dat/proto))

(in-package :net/tests)

(defsuite :net)
(in-suite :net)
(in-readtable :std)
(deftest sanity ())

(deftest sans-io ()
  (define-protocol mockz () (data) :version 2 :features (list :foo :bar :baz))
  (is (eql 'mockz (protocol-name (make-instance 'mockz))))
  (is (null (protocol-features (make-instance 'sans-io-protocol :features nil))))
  (is (= 3 (length (protocol-features (make-instance 'mockz)))))
  (is (= 2 (protocol-version (make-instance 'mockz))))
  (defclass mock-transport-config (transport-config)
    (max-bidi-streams
     max-uni-streams
     max-idle-timeout
     rx-window
     tx-window
     (packet-threshold :initform 3)
     (time-threshold :initform (/ 9 8))
     (initial-rtt :initform 333)
     initial-mtu
     min-mtu
     (datagram-rx-buffer-size :initform 1250000)
     (datagram-tx-buffer-size :initform (* 1024 1024))))
  (defclass mock-server-config (server-config)
    ((port :initarg :port :initform 0)))
  (defclass mock-client-config (client-config)
    ((port :initarg :port :initform 0)))
  (defclass mock-endpoint (endpoint)
    ((tx :initarg :tx)
     (rx :initarg :rx))
    (:default-initargs
     :server (make-instance 'mock-server-config)))
  (let ((ent (make-instance 'mock-endpoint)))
    (is (equal (class-name (class-of ent)) 'mock-endpoint))))

(deftest dns ())

(deftest tcp ()
  (with-tcp-client (client)
    (is (typep client 'sb-bsd-sockets:inet-socket))))

(deftest udp ()
  (with-udp-client (client)
    (is (typep client 'sb-bsd-sockets:inet-socket))))

(deftest tlv ()
  (is (= 4 (length (serialize (make-instance 'tlv :type 0 :length 1 :value #(1)) :bytes)))))

(deftest osc ())

(deftest crew (:disabled t)
  (let ((pool (make-worker-pool (make-instance 'crew-connection-info :host-name "localhost" :port 9999)
                                (list (make-instance 'crew-connection-info :host-name "localhost" :port 10000))
                                #'connect-worker)))
    (let* ((worker-count (if (null pool) 1 (worker-count pool)))
           (work '(cons 1 2))
           (expected-result (make-list worker-count :initial-element '(1 . 2)))
           (count 0)
           (count-lock (make-mutex :name "count")))
      (flet ((result-done (position element)
               (with-mutex (count-lock)
                 (incf count)
                 (is (equal (nth position expected-result) element)))))
        (is (equal (eval-form-all-workers pool work :replay-required nil) expected-result))
        (is (equal (eval-form-all-workers pool work :result-done #'result-done :replay-required nil)
                   expected-result))
        (is (= count worker-count))))))

;; (defun test-eval-form-repeatedly (pool)
;;   (let ((worker-count (if (null pool) 1 (worker-count pool))))
;;     (is (equal (eval-form-repeatedly pool 0 '(constantly 42)) '()))
;;     (is (equal (eval-form-repeatedly pool 10 '(constantly (cons 1 2)))
;;                (make-list 10 :initial-element (cons 1 2))))
;;     (is (equal (eval-form-repeatedly pool 20 '(constantly (cons 3 4))
;;                                      :worker-count (floor (/ worker-count 2)))
;;                (make-list 20 :initial-element (cons 3 4))))
;;     (is (equal (eval-form-repeatedly pool 30 '(constantly (cons 5 6)) :worker-count 0)
;;                (make-list 30 :initial-element (cons 5 6))))))

;; (defun test-parallel-mapcar (pool)
;;   (let ((input '(100 200 300))
;;         (expected-result '((100 . 1) (200 . 1) (300 . 1)))
;;         (count 0))
;;     (flet ((result-done (position element)
;;              (incf count)
;;              (is (equal (nth position expected-result) element))))
;;       (is (equal (parallel-mapcar pool (lambda (x) `(cons ,x 1)) input) expected-result))
;;       (is (equal (parallel-mapcar pool (lambda (x) `(cons ,x 1)) input #'result-done)
;;                  expected-result))
;;       (is (= count (length expected-result))))))

;; (defun test-parallel-reduce (pool)
;;   (is (equal (parallel-reduce pool
;;                               (lambda (x) `(list ,x 1))
;;                               '(100 200 300)
;;                               '(a b c)
;;                               #'append)
;;              '(a b c 100 1 200 1 300 1))))

;; (defun test-eval-repeatedly-async-state (pool)
;;   (let ((expected-state 10)
;;         (update-count 0)
;;         (work-form '(lambda (state)
;;                      ;; Return results slowly so we don't create huge result lists.
;;                      (sleep 0.1)
;;                      (* state state))))
;;     (flet ((update-state (state results)
;;              (is (= state expected-state))
;;              (is (not (null results)))
;;              (dolist (result results)
;;                (is (or (= result (expt state 2))
;;                        (= result (expt (1- state) 2))
;;                        (= result (expt (- state 2) 2)))))
;;              ;; Allow time for several results to accumulate.
;;              (sleep 0.5)
;;              (values (incf expected-state) (> (incf update-count) 3) t)))
;;       (eval-repeatedly-async-state pool work-form 10 #'update-state :worker-count 0)
;;       (setf expected-state 10
;;             update-count 0)
;;       (eval-repeatedly-async-state pool work-form 10 #'update-state))))

(deftest http ()
  (let ((req (make-http-request))
      (cb (make-callbacks)))
  (parse-request
   req cb
   (sb-ext:string-to-octets #"GET /cookies HTTP/1.1
Host: 127.0.0.1:8080
Connection: keep-alive
Cache-Control: max-age=0Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.56 Safari/537.17
Accept-Encoding: gzip,deflate,sdch
Accept-Language: en-US,en;q=0.8
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3
Cookie: name=wookie

"#))
    (is cb)
    (is req)))

(deftest req ())

(deftest fetch ())

(deftest cookies ())
