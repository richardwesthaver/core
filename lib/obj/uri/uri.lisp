;;; lib/obj/uri.lisp --- URIs -*- mode: common-lisp; -*-

;; URI/IRI/URN support based on Franz's URI support library for
;; Allegro.

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

;;; Code:
(in-package :obj/uri)

#+nil
(eval-when (:load-toplevel)
  (pushnew :rfc3986 *features*)
  (pushnew :rfc6874 *features*)
  (pushnew :rfc8141 *features*))

;; This does not persist past the end of compile-file
(eval-when (:compile-toplevel) (declaim (optimize (speed 3))))

(eval-always
  (defvar *strict-parse* t))

;; uri-host is computed and cached.  See the hand-written method below.
;; uri-ipv6 and uri-zone-id are read-only by users, so they are in the
;;   internal section below.
(defclass uri ()
  (;; special slots..
   ;; These slots are special: when they are changed, the string and
   ;; hashcode slots need to be set to nil.  For path, parsed-path also
   ;; needs to be set to nil.  See define-special-uri-slot-setters below.
   (scheme :initarg :scheme :initform nil :accessor uri-scheme)
   (userinfo :initarg :userinfo :initform nil :accessor uri-userinfo)
   (port :initarg :port :initform nil :accessor uri-port)
   (path :initarg :path :initform nil :accessor uri-path)
   (query :initarg :query :initform nil :accessor uri-query)
   (fragment :initarg :fragment :initform nil :accessor uri-fragment)
   ;; ..end special slots
   (plist :initarg :plist :initform nil :accessor uri-plist)
   ;; internal slots
   (%host ;; where part of the value for uri-host is stored
    ;; The values stored here are for URIs with names or IPv4 addresses.
    ;; IPv6 addresses are stored in the .ipv6 and .zone-id slots.
    ;; NOTE:
    ;;  I'm conflicted over the fact that .host is both computed and NOT
    ;;  computed.  It is computed for IPv6, but it holds the actual values
    ;;  from the parse for names or IPv4 addresses.  It might be a tiny bit
    ;;  more clear to have a separate slot for the computed value, but
    ;;  would that extra clarity be worth the extra space at runtime?
    :initarg :host :initform nil :accessor %uri-host)
   (%ipv6 ;; the pure IPv6 portion of the uri-host, nil otherwise
    ;; This value is the actual IPv6 address that would be suitable for use
    ;; in networking functions.  It does NOT include the zone-id or the
    ;; URI [] syntax.
    :initarg :ipv6 :initform nil :accessor %uri-ipv6)
   (%zone-id ;; used if IPv6 has a zone ID
    :initarg :zone-id :initform nil :accessor %uri-zone-id)
   (escaped ;; non-nil if parsed input contained pct encoded characters
    :initarg :escaped :initform nil :accessor uri-escaped)
   (string ;; the cached printable representation of the URI
    ;; It might be different than the original string, because of percent
    ;; encoding.  Use of slot setf methods may reset this slot to nil,
    ;; causing it to be recomputed when needed.
    :initarg :string :initform nil :accessor uri-string)
   (parsed-path ;; the cached parsed representation of the URI path
    :initarg :parsed-path
    :initform nil
    :accessor %uri-parsed-path)
   (hashcode ;; cached sxhash, so we don't have to compute it more than once
    :initarg :hashcode :initform nil :accessor uri-hashcode)))

#+has-clos-fixed-index-feature (:metaclass fixed-index-class)

;;; IRI
;; - The grammar for IRIs is identical to that of URIs, except the allowed
;;   character set for URIs is limited to ASCII, while IRIs characters can
;;   be from the sequence of characters from the Universal Character Set
;;   (Unicode/ISO 10646).
;; - The actual grammar differences are:
;;   - `unreserved' is now `iunreserved', which adds the alternation case
;;     `ucschar' (see ucscharp below).
;;   - `query' is now `iquery', which adds the alternation case
;;     `iprivate' (see iprivatep below).
;; - The IRI parser, string-to-iri, uses the URI parser, but it binds
;;   .iri-mode. to T, which changes how character validation is done.  In
;;   IRI mode, ucscharp and iprivatep are used in the appropriate places.
;;
;; See the comments for make-char-bitvector for more details.

(defclass iri (uri) ())

(defvar %iri-mode
  ;; Bound to T when we are parsing in IRI mode
  nil)

(defmethod uri-host ((uri uri))
  ;; Return the computed host for URI.  It is the value which could be used
  ;; by networking functions or programs to perform communication with the
  ;; resource designated by URI.
  (let ((host (%uri-host uri))
	ipv6 zone-id)
    ;; If HOST has a value, then use that.  Otherwise, if IPV6 has a value,
    ;; then return the IPv6 address, which will include the zone-id, if
    ;; non-nil.  Otherwise, return nil.
    (if* host
       thenret
     elseif (setq ipv6 (%uri-ipv6 uri))
       then ;; This setf clears the cached printed value (string slot)
	    (setf (%uri-host uri)
	          (if* (setq zone-id (%uri-zone-id uri))
		     then (concatenate 'string ipv6 "%" zone-id)
		     else ipv6)))))

;; It is by design there are no public setf methods for these
(defmethod uri-ipv6    ((uri uri)) (%uri-ipv6    uri))
(defmethod uri-zone-id ((uri uri)) (%uri-zone-id uri))

;; The .HOST slot is computed, for IPv6, or the actual name or IPv4
;; address.  To ensure all three slots are kept consistent, define a
;; function to set them.
(defun set-host (uri name-or-ipv4 ipv6 zone-id)
  (when (and name-or-ipv4 ipv6)
    (error "Both the IPv4/name and IPv6 values cannot be non-nil: ~s, ~s."
	   name-or-ipv4 ipv6))
  (setf (%uri-host    uri) name-or-ipv4
	(%uri-ipv6    uri) ipv6
	(%uri-zone-id uri) zone-id))

(defmethod (setf uri-host) (v (uri uri))
  (prog1
      (if* (null v)
	 then (set-host uri nil nil nil)
       elseif (stringp v)
	 then (multiple-value-bind (found whole ipv6 zone-id)
		  ;; This embodies knowledge of the URI IPv6 syntax
		  (ppcre:scan "^(.*:.*?)(%.*)?$" v)
		(declare (ignore whole))
		(if* found
		   then (set-host uri nil ipv6 zone-id)
		   else (set-host uri v nil nil))
		v)
	 else (error "host value must be a string: ~s." v))
    ;; This slot doesn't use clear-computed-uri-slots, so we must do this
    ;; manually:
    (setf (uri-string uri) nil)
    (setf (uri-hashcode uri) nil)))

(defclass urn (uri)
  ;; NOTE: the q-component is stored in the `query' slot and the
  ;;       f-component is stored in the `fragment' slot of the of the
  ;;       parent class (uri).
  ;; The slots below have no place in the parent class.
  ((nid :initarg :nid :initform nil :accessor urn-nid)
   (nss :initarg :nss :initform nil :accessor urn-nss)
   ;; q-component is stored in the `query'
   ;; f-component is stored in the `fragment'
   (r-component ;; ignored in comparisons
    :initarg :r-component :initform nil :accessor urn-r-component)))

#+has-clos-fixed-index-feature (:metaclass fixed-index-class)

(defmethod make-load-form ((self uri) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-name (class-of self))
     :scheme ,(uri-scheme self)
     :host ,(%uri-host self)
     :ipv6 ,(%uri-ipv6 self)
     :zone-id ,(%uri-zone-id self)
     :userinfo ,(uri-userinfo self)
     :port ,(uri-port self)
     :path ',(uri-path self)
     :query ,(uri-query self)
     :fragment ,(uri-fragment self)
     :plist ',(uri-plist self)
     :string ,(uri-string self)
     :parsed-path ',(%uri-parsed-path self)))

(defmethod make-load-form ((self urn) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-name (class-of self))
     :scheme ,(uri-scheme self)
     :host ,(%uri-host self)
     :ipv6 ,(%uri-ipv6 self)
     :zone-id ,(%uri-zone-id self)
     :userinfo ,(uri-userinfo self)
     :port ,(uri-port self)
     :path ',(uri-path self)
     :query ,(uri-query self)		; q-component
     :fragment ,(uri-fragment self)	; f-component
     :plist ',(uri-plist self)
     :string ,(uri-string self)
     :parsed-path ',(%uri-parsed-path self)
   ;;; URN-specific:
     :nid ,(urn-nid self)
     :nss ,(urn-nss self)
     :r-component ,(urn-r-component self)))

(define-condition uri-condition () ())
(define-condition uri-error (uri-condition error) ())
(define-condition uri-parse-error (parse-error uri-error)
  ((string :initarg :string :reader uri-parse-error-string)))

(defmethod uri-p ((thing uri)) t)
(defmethod uri-p ((thing t)) nil)

(defmethod iri-p ((thing iri)) t)
(defmethod iri-p ((thing t)) nil)

(defun copy-uri (uri
		 &key place
		      (scheme (when uri (uri-scheme uri)))
		      (host (when uri (%uri-host uri)))
		      (ipv6 (when uri (%uri-ipv6 uri)))
		      (zone-id (when uri (%uri-zone-id uri)))
		      (userinfo (when uri (uri-userinfo uri)))
		      (port (when uri (uri-port uri)))
		      (path (when uri (uri-path uri)))
		      (parsed-path
		       (when uri (copy-list (%uri-parsed-path uri))))
		      (query (when uri (uri-query uri)))
		      (fragment (when uri (uri-fragment uri)))
		      (plist (when uri (copy-list (uri-plist uri))))
		      (class (when uri (class-of uri)))
		 &aux (escaped (when uri (uri-escaped uri))))
  (if* place
     then (setf (uri-scheme place) scheme)
	  (set-host place host ipv6 zone-id)
	  (setf (uri-userinfo place) userinfo)
	  (setf (uri-port place) port)
	  (setf (uri-path place) path)
	  (setf (%uri-parsed-path place) parsed-path)
	  (setf (uri-query place) query)
	  (setf (uri-fragment place) fragment)
	  (setf (uri-plist place) plist)
	  (setf (uri-escaped place) escaped)
	  (setf (uri-hashcode place) nil)
	  place
   elseif (eq 'uri class)
     then ;; allow the compiler to optimize the call to make-instance:
	  (make-instance 'uri
	    :scheme scheme :host host :ipv6 ipv6 :zone-id zone-id
	    :userinfo userinfo :port port
	    :path path :parsed-path parsed-path
	    :query query :fragment fragment :plist plist
	    :escaped escaped :string nil :hashcode nil)
     else (make-instance class
	    :scheme scheme :host host :ipv6 ipv6 :zone-id zone-id
	    :userinfo userinfo :port port
	    :path path :parsed-path parsed-path
	    :query query :fragment fragment :plist plist
	    :escaped escaped :string nil :hashcode nil)))

(defmethod uri-parsed-path ((uri uri))
  (let ((p (uri-path uri)))
    (when p
      (if* (%uri-parsed-path uri)
	 thenret
	 else (setf (%uri-parsed-path uri)
		    (parse-path (uri-path uri) (uri-escaped uri)))))))

(defmethod (setf uri-parsed-path) (path-list (uri uri))
  (if* (null path-list)
     then (setf (uri-path uri) nil)
	  (setf (%uri-parsed-path uri) nil)
	  path-list
     else (when (not (and (consp path-list)
			  (or (member (car path-list) '(:absolute :relative)
				      :test #'eq))))
	    (error "internal error: path-list is ~s." path-list))
	  (setf (uri-path uri) (render-parsed-path path-list t))
	  (setf (%uri-parsed-path uri) path-list)
	  path-list))

(defun uri-authority (uri)
  (when (uri-host uri)
    (let ((*print-pretty* nil))
      (format nil "~@[~a@~]~a~@[:~a~]" (uri-userinfo uri)
	      (uri-host uri) (uri-port uri)))))

(defun uri-nid (uri)
  (if* (equalp "urn" (uri-scheme uri))
     then ;; Intentionally did not use .uri-host:
	  (uri-host uri)
     else (error "URI is not a URN: ~s." uri)))

(defun uri-nss (uri)
  (if* (equalp "urn" (uri-scheme uri))
     then (uri-path uri)
     else (error "URI is not a URN: ~s." uri)))

(defmethod urn-q-component ((urn urn)) (uri-query urn))
(defmethod urn-f-component ((urn urn)) (uri-fragment urn))

(defmethod uri ((thing uri))     thing)
(defmethod uri ((thing string)) (parse-uri thing))
(defmethod uri ((thing t))      (error "Cannot coerce ~s to a uri." thing))

;; (parse-uri-string-rfc3986 "https://test.com")
