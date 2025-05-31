;;; obj/uri/parse.lisp --- URI Parsers

;;

;;; Code:
(in-package :obj/uri)

(eval-when (:compile-toplevel :execute :load-toplevel)
  ;; Generate the parser for URI or IRI.  The only difference is the name
  ;; of the parser and for IRIs the binding of %iri-mode to T.
  (defmacro gen-xri-parser (name irip)
    `(defun ,name (string
                   &aux ,@(when irip '((%iri-mode t)))
                        (end (length string))
                        (.pct-encoded. nil)
                        real-host ipv6 zone-id)
       (declare (optimize (safety 0))
                (fixnum end)) 

       (check-xri-string string)

       (multiple-value-bind (i scheme userinfo host port path query fragment)
           (state-absolute-uri string 0 end)
         (when i
           (if* (and host (consp host))
              then (setq real-host (first host))
                   (setq ipv6 (second host))
                   (setq zone-id (third host))
              else (setq real-host host))
           (when port
             (setq port (xval string port))
             (setq port (parse-integer port :radix 10)))
           (return-from ,name
             (values (xval string scheme)
                     (xval string real-host)
                     (xval string userinfo)
                     port
                     (xval string path)
                     (xval string query)
                     ;; This is only non-nil for URNs
                     (xval string fragment)
                     .pct-encoded.
                     (xval string ipv6)
                     (xval string zone-id)))))

       (multiple-value-bind (i scheme userinfo host port path query fragment)
           (state-uri-reference string 0 end)
         (when i
           (if* (and host (consp host))
              then (setq real-host (first host))
                   (setq ipv6 (second host))
                   (setq zone-id (third host))
              else (setq real-host host))
           (when port
             (setq port (xval string port))
             (setq port (parse-integer port :radix 10)))
           (return-from ,name
             (values (xval string scheme)
                     (xval string real-host)
                     (xval string userinfo)
                     port
                     (xval string path)
                     (xval string query)
                     (xval string fragment)
                     .pct-encoded.
                     (xval string ipv6)
                     (xval string zone-id)))))

       (uri-parse-error string "Couldn't parse uri: ~s." string))))

(defun uri-parse-error (string format-string &rest format-arguments)
  (error 'uri-parse-error
         :string string
         :format-control format-string
         :format-arguments format-arguments))

(gen-xri-parser parse-uri-string-rfc3986 nil)
(gen-xri-parser parse-iri-string-rfc3987 :iri-mode)

;; TODO fix string escapes
(defun parse-uri (thing &key (class 'uri) (escape t))
  ;; Parse THING into a URI object, an instance of CLASS.
  ;;
  ;; If ESCAPE is non-nil, then decode percent-encoded characters in places
  ;; where they can legally appear, into the raw characters.  The exception
  ;; to this is when those characters are reserved for the component in
  ;; which they appear, and in this case the percent-encoded character
  ;; stays encoded.

  (when (uri-p thing) (return-from parse-uri thing))

  (multiple-value-bind (scheme host userinfo port path query fragment
                        pct-encoded ipv6 zone-id)
      (parse-uri-string-rfc3986 thing)
    (when scheme
      (setq scheme
        (cond
         ;; Ordered from most common to least, and the set of known schemes
         ;; hardwired for efficiency.
         ((string-equal scheme "https") :https)
         ((string-equal scheme "http") :http)
         ((string-equal scheme "ftp") :ftp)
         ((string-equal scheme "file") :file)
         ((string-equal scheme "urn") :urn)
         ((string-equal scheme "telnet") :telnet)
         (t
          (intern (funcall
                   (case *print-case*
                     ((:upcase)
                      #'string-upcase)
                     ((:downcase)
                      #'string-downcase))
                   scheme)
                  (load-time-value (find-package :keyword)))))))

    (when (and scheme (eq :urn scheme))
      (return-from parse-uri
        (make-instance 'urn :scheme scheme :nid host :nss path
                       :query query :fragment fragment
                       :r-component userinfo)))

    (when (and escape host)
      (setq host (percent-decode-string host *reg-name-bitvector*)))
    (when (and escape userinfo)
      (setq userinfo (percent-decode-string userinfo *userinfo-bitvector*)))
    (when port
      (when (not (numberp port)) (error "port is not a number: ~s." port))
      (when (not (plusp port))
        (error "port is not a positive integer: ~d." port))
      ;; Use `eql' instead of `=' so that scheme's other than the small set
      ;; below are possible.
      (when (eql port (case scheme
                        (:http 80)
                        (:https 443)
                        (:ftp 21)
                        (:telnet 23)))
        (setq port nil)))
    (when (= 0 (length path))
      (setq path nil))
    (when (and escape path)
      (setq path (percent-decode-string path *pchar-bitvector*)))
    (when (and escape query)
      (setq query
        (percent-decode-string query
                               (if* *strict-parse*
                                  then *decode-query-bitvector-strict*
                                  else *decode-query-bitvector-non-strict*))))
    (when (and escape fragment)
      (setq fragment
        (percent-decode-string fragment
                               (if* *strict-parse*
                                  then *fragment-bitvector-strict*
                                  else *fragment-bitvector-non-strict*))))
    (if* (eq 'uri class)
       then ;; allow the compiler to optimize the make-instance call:
            (make-instance 'uri
              :scheme scheme
              :host host
              :ipv6 ipv6
              :zone-id zone-id
              :userinfo userinfo
              :port port
              :path path
              :query query
              :fragment fragment
              :escaped (when escape pct-encoded))
       else ;; do it the slow way:
            (make-instance class
              :scheme scheme
              :host host
              :userinfo userinfo
              :port port
              :path path
              :query query
              :fragment fragment
              :escaped (when escape pct-encoded)))))

  (defmacro gen-string-to-xri (name parser class)
    `(defun ,name (string)
       ;; Parse STRING as a xRI and either signal an error if it cannot be
       ;; parsed or return the xRI object.  This function differs from
       ;; parse-uri in that the query is not decoded.  The knowledge of how
       ;; to properly decode the query is outside the bounds of RFC 3986/7.
       (multiple-value-bind (scheme host userinfo port path query fragment
                        pct-encoded ;; non-nil if any %xx in any slot
                        ipv6 zone-id)
      (,parser string)

    (when scheme
      (setq scheme
        (cond
         ;; Ordered from most common to least, and the set of known schemes
         ;; hardwired for efficiency.
         ((string-equal scheme "https") :https)
         ((string-equal scheme "http") :http)
         ((string-equal scheme "ftp") :ftp)
         ((string-equal scheme "file") :file)
         ((string-equal scheme "urn") :urn)
         ((string-equal scheme "telnet") :telnet)
         (t
          (intern (funcall
                   (case *print-case*
                     ((:upcase)
                      #'string-upcase)
                     ((:downcase)
                      #'string-downcase))
                   scheme)
                  (load-time-value (find-package :keyword)))))))

    (when (and scheme (eq :urn scheme))
      (return-from ,name
        ;; NOTE: for now, we treat URNs like parse-uri, and do no
        ;; decoding.
        (make-instance 'urn :scheme scheme :nid host :nss path
                       :query query :fragment fragment
                       :r-component userinfo)))

    (when (and pct-encoded host)
      (setq host (percent-decode-string host *reg-name-bitvector*)))

    (when (and pct-encoded userinfo)
      (setq userinfo (percent-decode-string userinfo *userinfo-bitvector*)))

    (when port
      (when (not (numberp port)) (error "port is not a number: ~s." port))
      (when (not (plusp port))
        (error "port is not a positive integer: ~d." port))
      ;; Use `eql' instead of `=' so that scheme's other than the small set
      ;; below are possible.
      (when (eql port (case scheme
                        (:http 80)
                        (:https 443)
                        (:ftp 21)
                        (:telnet 23)))
        (setq port nil)))

    (when (= 0 (length path))
      (setq path nil))
    (when (and pct-encoded path)
      (setq path (percent-decode-string path *pchar-bitvector*)))

    ;; query is left alone

    (when (and pct-encoded fragment)
      (setq fragment
        (percent-decode-string fragment
                               (if* *strict-parse*
                                  then *fragment-bitvector-strict*
                                  else *fragment-bitvector-non-strict*))))

    (make-instance ,class
      :scheme scheme
      :host host
      :ipv6 ipv6
      :zone-id zone-id
      :userinfo userinfo
      :port port
      :path path
      :query query
      :fragment fragment
      :escaped pct-encoded))))

(gen-string-to-xri string-to-uri parse-uri-string-rfc3986 'uri)
(gen-string-to-xri string-to-iri parse-iri-string-rfc3987 'iri)

(defun parse-path (path-string escape)
  (do* ((xpath-list (uiop:split-string path-string :separator '(#\/)))
        (path-list
         (let (#+mswindows temp #+mswindows c)
           (cond ((string= "" (car xpath-list))
                  (setf (car xpath-list) :absolute))
                 (t (push :relative xpath-list)))
           xpath-list))
        (pl (cdr path-list) (cdr pl))
        segments)
      ((null pl) path-list)

    (if* (symbolp (car pl))
       then ;; Only happens on Windows when we see a path with a drive
            ;; letter.  The lack of #+mswindows doesn't matter here.
            nil
     elseif (cdr (setq segments
                   (if* (string= "" (car pl))
                      then '("")
                      else (uiop:split-string (car pl) :separator '(#\:)))))
       then ;; there is a param
            (setf (car pl)
              (mapcar #'(lambda (s)
                          (if* escape
                             then (percent-decode-string s nil)
                             else s))
                      segments))
       else ;; no param
            (setf (car pl)
              (if* escape
                 then (percent-decode-string (car segments) nil)
                 else (car segments))))))
