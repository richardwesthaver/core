(defpackage :net/tests
  (:use :rt :std :cl :net :sb-thread))

(in-package :net/tests)

(defsuite :net)
(in-suite :net)
(in-readtable :std)

(deftest dns ()
  (istype '(simple-array (unsigned-byte 8) (4)) (make-inet-address (resolve "compiler.company")))
  (istype 'string (hostname (resolve "google.com"))))

(deftest tcp ()
  (with-tcp-client (client)
    (is (typep client 'inet-socket))
    (is (= (get-protocol-by-name :tcp)
           (socket-protocol client)))))

(deftest udp ()
  (with-udp-client (client)
    (istype 'inet-socket client)
    (is (= (get-protocol-by-name :udp)
           (socket-protocol client)))))

(deftest tlv ()
  (is= 4 (length (serialize (make-instance 'tlv :type 0 :length 1 :value #(1)) :bytes))))

(deftest osc ()
  (isequalp (net/codec/osc::encode-int32 16843009) #(1 1 1 1))
  (isequalp (net/codec/osc::decode-int32 #(1 1 1 1)) 16843009)
  (isequalp (net/codec/osc::decode-string #(110 117 108 108 32 112 97 100 100 101 100 0))
            "null padded")
  (isequalp (net/codec/osc::encode-blob #(1 1 1 1)) #(0 0 0 4 1 1 1 1))
  (isequalp (net/codec/osc::encode-timetag :now) #(0 0 0 0 0 0 0 1))
  (isequalp (net/codec/osc::encode-int64 16843009) #(0 0 0 0 1 1 1 1))
  (isequalp (net/codec/osc::enc-float32 1.00001) #(63 128 0 84))
  (isequalp (net/codec/osc::decode-float64 (octets-to-integer (octets 64 55 25 153 153 153 153 154))) 23.1d0)
  (isequalp '("/test/int" -1)
            (net/codec/osc:decode-osc-message 
             #(47 116 101 115 116 47 105 110 116 0 0 0 44 105 0 0 255 255 255 255))))

(deftest http ()
  (let ((req (http:make-http-request))
        (cb (http:make-callbacks)))
    (http:parse-request
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
  (cry/tls:ensure-ssl)
  (istype 'net/req::keep-alive-chunked-stream
          (req:get (uri:uri "https://compiler.company") :force-binary t :want-stream t :keep-alive t))
  (istype 'string (req:get (uri:uri "https://compiler.company")))
  (istype 'octet-vector (req:get (uri:uri "https://compiler.company") :force-binary t)))

(deftest cookies ()
  (let ((cookies (net/cookie:make-cookie-jar))
        (cookie (net/cookie:make-cookie)))
    (net/cookie:merge-cookies cookies #(cookie))
    (is (= 1 (length (net/cookie:cookie-jar-cookies cookies))))
    (is (net/cookie:cookie= cookie (net/cookie:make-cookie)))
    (is (stringp (net/cookie:write-cookie-header (list cookie))))))

(deftest srv ()
  (let ((srv (make-instance 'udp-service)))
    (issubtype 'net-service (type-of srv))
    (istype 'net/srv/http:http-service (make-instance 'net/srv/http:http-service))
    (istype 'net/srv/http:https-service (make-instance 'net/srv/http:https-service))
    (istype 'net/srv/udp:udp-service (make-instance 'net/srv/udp:udp-service))))

;; TODO 2025-10-18: 
(deftest swank (:skip :todo))

(deftest netlink ()
  (let ((ns (make-instance 'netlink-socket)))
    (istype 'netlink-socket ns)
    (is= io/socket::+size-of-sockaddr-nl+ (size-of-sockaddr ns))
    (let ((a (make-sockaddr-for ns)))
      (istype '(alien (* io/socket:sockaddr-nl)) a)
      (multiple-value-bind (pid grp) (bits-of-sockaddr ns a)
        (istype 'positive-integer pid)
        (istype 'positive-integer grp)
        (free-sockaddr-for ns a)))))

(deftest socket ()
  (with-open-socket ((s c) :port 443 :connect "compiler.company" :bind '(#(0 0 0 0) 0) :close t) 
    (istype 'client s) 
    (istype 'stream c)))
