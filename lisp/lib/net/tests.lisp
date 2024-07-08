(defpackage :net/tests
  (:use :rt :std :cl :net :sb-concurrency :sb-thread :dat/proto))

(in-package :net/tests)

(defsuite :net)
(in-suite :net)
(in-readtable :std)

(deftest sanity ())

(deftest dns ()
  (is (stringp (resolve "compiler.company"))))

(deftest tcp ()
  (with-tcp-client (client)
    (is (typep client 'sb-bsd-sockets:inet-socket))))

(deftest udp ()
  (with-udp-client (client)
    (is (typep client 'sb-bsd-sockets:inet-socket))))

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
  (is (req:get (uri:uri "https://compiler.company/index.html"))))

(deftest fetch ()
  (is (fetch:download "https://compiler.company/index.html" "/tmp/index.html"))
  (is (delete-file "/tmp/index.html")))

(deftest cookies ()
  (let ((cookies (net/cookie:make-cookie-jar))
        (cookie (net/cookie:make-cookie)))
    (net/cookie:merge-cookies cookies #(cookie))
    (is (= 1 (length (net/cookie:cookie-jar-cookies cookies))))
    (is (net/cookie:cookie= cookie (net/cookie:make-cookie)))
    (is (stringp (net/cookie:write-cookie-header (list cookie))))))


(deftest srv ()
  (is (pathnamep (net/srv:default-web-directory))))
