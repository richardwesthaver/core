;;; tests.lisp --- OBJ Tests

;;; Code:
(defpkg :obj/tests
  (:use :cl :std :rt :obj :uuid :url :std/macs :id :ast
    :dynamic :fast :sealed :stealth :stored :uri :color 
    :db :store :schema :cmd))

(in-package :obj/tests)

(defsuite :obj)
(in-suite :obj)

;;; Color
(defun eps= (a b &optional (eps 1e-10))
  (<= (abs (- a b)) eps))

(defun rgb= (rgb1 rgb2 &optional (eps 1e-10))
  "Compare RGB colors for (numerical) equality."
  (let ((r1 (red rgb1))
        (g1 (green rgb1))
        (b1 (blue rgb1))
        (r2 (red rgb2))
        (g2 (green rgb2))
        (b2 (blue rgb2)))
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
    (is (equal "#12345678" (print (print-hex-rgb rgb :alpha 0.47))))
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

(deftest palette ()
  (istype 'hash-table (make-palette :foo))
  (istype 'hash-table (find-palette :foo))
  (remove-palette :foo)
  (isnt (find-palette :foo))
  (with-palette :modus-operandi
    (is (obj/color::rgb= (rgb 0 0 0) (get-color :fg-main)))))

(deftest theme (:skip :todo))

;;; Id
(deftest ids ()
  (is (= (reset-id t) (reset-id '(1 2 3))))
  (is (not (equalp (make-id nil) (make-id nil)))))

;;; Uuid
(deftest uuids ()
  (macrolet ((is-uuid (obj) `(is (typep ,obj 'uuid))))
    (is-uuid (make-v1-uuid))
    (is-uuid (make-v4-uuid))))

;;; Trees
(deftest generic-tree ()
  (let ((tree (make-binary-node
               0 
               (make-binary-node 1 (make-tree-node 0) (make-tree-node 1))
               (make-binary-node 2 (make-tree-node 2) (make-tree-node 3)))))
    (is (typep tree 'binary-node))))

(deftest btree (:skip :todo)
  ;; FIX 2025-02-27: 
  (is (make-instance 'btree:btree :store *default-store*)))

;;; Graphs
(deftest basic-graph ()
  "Test basic graph functionality."
  (let ((g1 (make-instance 'graph:simple-graph)))
    (is (typep g1 'graph:graph))
    (graph:add-node g1 0)
    (graph:add-node g1 1)
    (graph:add-edge g1 '(0 1))
    ;; graph is undirected, so this is no-op
    (graph:add-edge g1 '(1 0))
    (is (equiv (copy-object g1) g1))
    ;; FIX 2025-10-30: 
    ;; (isnt (shortest-path g1 0 1))
    ;; and only 1 edge exists
    (is (= 2 (length (hash-table-keys (graph:edges g1)))))
    (let ((g2 (make-instance 'graph:simple-directed-graph)))
      (is (typep g2 'graph:directed-graph))
      (graph:add-node g2 (make-instance 'graph:vertex :id :baz))
      (graph:add-node g2 (make-instance 'graph:vertex :id :buz))
      (graph:add-edge g2 (make-instance 'graph:directed-edge :in :baz :out :buz))
      ;; graph is directed, so this is a unique edge
      (graph:add-edge g2 '(:buz :baz))
      ;; 2 edges exist
      (is (= 2 (length (hash-table-keys (graph:edges g2)))))
      ;; (shortest-path g2 :buz :baz)
      ;; (graph:add-node g1 g2)
      ;; (is (graph::has-node-p g1 g2))
      ;; (graph::delete-node g1 g2)
      ;; (is (not (graph::has-node-p g1 g2)))
      )))

;;; Uri
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

(deftest project ()
  (istype 'simple-project (make-project (gensym))))

;;; Store
(defclass test-store (store) ())
(deftest store ())

;;; Cache
(defun test-provider (key)
  "A provider returns two values: The data for the element, and the element's size."
  (values (format nil "value for ~a" key)
	  key))

(deftest simple-cache ()
  (let* ((acc (make-instance 'max-accumulator :value 0))
         (cache (make-cache 100 #'test-provider :policy :lru :cleanup (lambda (k) (istype 'string k)))))
    (is= (cache:cache-max-size cache) 100)
    (with-cache c (cache 42)
      (is= 42 (cache-size cache))
      (is= 1 (cache-count cache)))
    (is (cache-remove cache 42))
    (cache-fetch cache 10)
    (cache-fetch cache 20)
    (cache-fetch cache 30)
    (cache-flush cache)
    ;; (iszero (cache-count cache)) ;=3.. shouldn't this be zero?
    (iszero (cache-size cache))))

;;; Schema
(deftest simple-schema ()
  (let ((s (schema:make-simple-schema "foobar"
             (make-field :name :foo :type 'string)
             (make-field :name :bar :type 'octet-vector))))
    (istype 'simple-schema s)
    (is= 2 (length (fields s)))))

;;; Cmd
(defmacro ct (form &environment env)
  (let ((toeval `(let ((lexenv (quote ,env)))
                   ,form)))
    `(quote ,(eval toeval))))

(deftest interactive ()
  (locally (declare (interactive foo))
    (isequal '(foo) (ct (declaration-information 'interactive lexenv)))
    ;; (isequal '(foo) (cmd::%with-interactive i i))
    )
  (define-command-type (:test test1) (a) a)
  (define-command-type (:test test2) (b) (declare (ignore b)) "FOO")
  (is (command-types :test))
  (defcommand (:test foo) (a b &optional c)
      (declare (interactive test1 test2)) ; unspecified optional C
    (is a) (is b) (isnt c))
  (is (commands :test))
  (with-commands :test 
    (let ((foo (command 'foo)))
      (istype 'function foo)
      (is (commandp 'foo))
      (funcall foo 1 2 nil)
      (function-lambda-list foo))))
      ;; TODO
      ;; (call-interactively 'foo "1 2")

(deftest time ()
  (let ((time (now)))
    (is (timestamp= (timestamp-from-alien (timestamp-to-alien time)) time
                    (timestamp-from-integer (timestamp-to-integer time))))))
