;;; pkg.lisp --- SSL

;; 

;;; Code:
(pkg:defpkg :cry/tls
  (:nicknames :tls :ssl)
  (:use :cl :std :sb-gray :io :config :sb-alien :cry-int :openssl :dat/asn1)
  (:shadowing-import-from :std/rand :random-bytes)
  (:export :*ssl-cipher-list* :*ssl-buffer-size* :*ca-bundle* 
   :*no-ssl*
   :with-global-context
   :make-ssl-context
   :ensure-ssl
   :make-ssl-client-stream
   :make-ssl-server-stream
   :ssl-stream-x509-certificate
   #:decode-der-octet-vector
   #:decode-certificate-from-file
   #:ssl-initialized-p))

