;;; obj/uri.lisp --- Univeral Resource Identifiers

;; Where convenient, this is a straight-forward copy of Franz's
;; NET.URI. the source is available here:
;; https://github.com/franzinc/uri/blob/master/uri.cl

;; For general URI information see RFC 3986.

;; For general IRI information see RFC 3987.

;; For general URN information see RFC 8141.

;; For IPv6 changes see RFC 6874.

;; examples of URIs:
#|
ftp://ftp.is.co.za/rfc/rfc1808.txt
https://www.ietf.org/rfc/rfc2396.txt
ldap://[2001:db8::7]/c=GB?objectClass?one
mailto:John.Doe@example.com
news:comp.infosystems.www.servers.unix
tel:+1-816-555-1212
telnet://192.0.2.16:80/
urn:oasis:names:specification:docbook:dtd:xml:4.1.2
|#

;; 

;;; Code:
(in-package :obj/uri)

;; (parse-uri-string-rfc3986 "https://test.com")

;; TODO
;; (defmacro do-all-uris ((var &optional uri-space result-form)
;; 		       &rest forms
;; 		       &environment env)
;;   "do-all-uris (var [[uri-space] result-form])
;;   		    {declaration}* {tag | statement}*
;; Executes the forms once for each uri with var bound to the current uri"
;;   (let ((f (gensym))
;; 	(g-ignore (gensym))
;; 	(g-uri-space (gensym))
;; 	(body (third (excl::parse-body forms env))))
;;     `(let ((,g-uri-space (or ,uri-space *uris*)))
;;        (prog nil
;; 	 (flet ((,f (,var &optional ,g-ignore)
;; 		  (declare (ignorable ,var ,g-ignore))
;; 		  (tagbody ,@body)))
;; 	   (maphash #',f ,g-uri-space))
;; 	 (return ,result-form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pushnew :rfc3986 *features*)
(pushnew :rfc6874 *features*)
(pushnew :rfc8141 *features*)
