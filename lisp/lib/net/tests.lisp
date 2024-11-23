(defpackage :net/tests
  (:use :rt :std :cl :net :sb-concurrency :sb-thread :dat/proto :sb-bsd-sockets))

(in-package :net/tests)

(defsuite :net)
(in-suite :net)
(in-readtable :std)

(deftest sanity ())

(deftest dns ()
  (is (stringp (resolve "compiler.company"))))

(deftest tcp ()
  (with-tcp-client (client)
    (is (typep client 'sb-bsd-sockets:inet-socket))
    (is (= (get-protocol-by-name :tcp)
           (socket-protocol client)))))

(deftest udp ()
  (with-udp-client (client)
    (istype 'sb-bsd-sockets:inet-socket client)
    (is (= (get-protocol-by-name :udp)
           (socket-protocol client)))))

(deftest tlv ()
  (is (= 4 (length (serialize (make-instance 'tlv :type 0 :length 1 :value #(1)) :bytes)))))

(deftest osc ())

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

(deftest req ()
  (istype 'net/req::keep-alive-chunked-stream
          (req:get (uri:uri "https://google.com") :force-binary t :want-stream t :keep-alive t))
  (istype 'string (req:get (uri:uri "https://example.com")))
  (istype 'octet-vector (req:get (uri:uri "https://example.com") :force-binary t)))

(deftest fetch ()
  (with-output-to-string (s)
    (write-sequence (req:get "https://google.com") s))
  (is (delete-file "/tmp/index.html")))

(deftest cookies ()
  (let ((cookies (net/cookie:make-cookie-jar))
        (cookie (net/cookie:make-cookie)))
    (net/cookie:merge-cookies cookies #(cookie))
    (is (= 1 (length (net/cookie:cookie-jar-cookies cookies))))
    (is (net/cookie:cookie= cookie (net/cookie:make-cookie)))
    (is (stringp (net/cookie:write-cookie-header (list cookie))))))

(deftest srv ()
  (istype 'service (make-instance 'service)))

