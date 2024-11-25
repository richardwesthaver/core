;;; obj/uri/print.lisp --- URI printers

;;

;;; Code:
(in-package :obj/uri)
(defvar *render-include-slash-on-null-path* nil) ;; rfe11850
(defvar *uri-schema-print-case* :downcase)
(defgeneric render-uri (uri &optional stream))
(defmethod render-uri ((uri uri) &optional stream
                       &aux (encode (uri-escaped uri))
                            (*print-pretty* nil)
                            res)
  (declare (optimize (safety 0)))
  (when (null (setq res (uri-string uri)))
    (setf (uri-string uri)
      (let ((scheme (uri-scheme uri))
            (host (%uri-host uri))
            (ipv6 (%uri-ipv6 uri))
            zone-id ;; don't compute until needed
            (userinfo (uri-userinfo uri))
            (port (uri-port uri))
            (path (uri-path uri))
            (query (uri-query uri))
            (fragment (uri-fragment uri)))
        (setq res
          (concatenate 'string
           (when scheme
             (case *uri-schema-print-case*
               ((:downcase)
                (string-downcase (symbol-name scheme)))
               ((:upcase)
                (symbol-name scheme))))
           (when scheme ":")
           (when (or host ipv6 (eq :file scheme) (eq :hdfs scheme))
             "//")
           (when userinfo
             (if* encode
                then (percent-encode-string userinfo *userinfo-bitvector*)
                else userinfo))
           (when userinfo "@")
           (if* ipv6
              then (if* (setq zone-id (%uri-zone-id uri))
                      then (concatenate 'string "[" ipv6 "%25" zone-id "]")
                      else (concatenate 'string "[" ipv6 "]"))
            elseif host
              then (if* encode
                      then (percent-encode-string host *reg-name-bitvector*)
                      else host))
           (when port (format nil ":~d" port))
           (if* path
              then path
            elseif (and *render-include-slash-on-null-path*
                        #|no path but:|# scheme host)
              then "/")
           (when query "?")
           (when query
             (if* encode
                then (percent-encode-string
                      query
                      (if* *strict-parse*
                         then *query-bitvector-strict*
                         else *query-bitvector-non-strict*))
                else query))
           (when fragment "#")
           (when fragment
             (if* encode
                then (percent-encode-string
                      fragment
                      (if* *strict-parse*
                         then *fragment-bitvector-strict*
                         else *fragment-bitvector-non-strict*))
                else fragment))))))

    ;; calculate this cached slot
    (uri-parsed-path uri))

  (if* stream
     then (princ res stream)
     else res))

(defmethod render-uri ((urn urn) &optional stream
                       &aux (*print-pretty* nil))
  ;; This doesn't do encoding because no decoding is done for URNs when
  ;; they are parsed.
  (when (null (uri-string urn))
    (setf (uri-string urn)
      (let ((nid (urn-nid urn))
            (nss (urn-nss urn))
            (r (urn-r-component urn))
            (q (urn-q-component urn))
            (f (urn-f-component urn)))
        (concatenate 'string "urn:" nid ":" nss
                     (when r "?+")
                 (when r r)
                 (when q "?=")
                 (when q q)
                 (when f "#")
                 (when f f)))))
  (if* stream
     then (write-string (uri-string urn) stream)
     else (uri-string urn)))

(defmethod uri-to-string ((uri uri)
                          &aux (encode (uri-escaped uri))
                               (*print-pretty* nil)
                               res)
  (declare (optimize (safety 0)))
  (when (null (setq res (uri-string uri)))
    (setf (uri-string uri)
      (let ((scheme (uri-scheme uri))
            (host (%uri-host uri))
            (ipv6 (%uri-ipv6 uri))
            zone-id ;; don't compute until needed
            (userinfo (uri-userinfo uri))
            (port (uri-port uri))
            (path (uri-path uri))
            (query (uri-query uri))
            (fragment (uri-fragment uri)))
        (setq res
          (concatenate 'string
           (when scheme
             (case *uri-schema-print-case*
               ((:downcase)
                (string-downcase (symbol-name scheme)))
               ((:upcase)
                (symbol-name scheme))))
           (when scheme ":")
           (when (or host ipv6 (eq :file scheme) (eq :hdfs scheme))
             "//")
           (when userinfo
             (if* encode
                then (percent-encode-string userinfo *userinfo-bitvector*)
                else userinfo))
           (when userinfo "@")
           (if* ipv6
              then (if* (setq zone-id (%uri-zone-id uri))
                      then (concatenate 'string "[" ipv6 "%25" zone-id "]")
                      else (concatenate 'string "[" ipv6 "]"))
            elseif host
              then (if* encode
                      then (percent-encode-string host *reg-name-bitvector*)
                      else host))
           (when port ":")
           (when port port)
           (if* path
              then path
            elseif (and *render-include-slash-on-null-path*
                        #|no path but:|# scheme host)
              then "/")
           (when query "?")
           query
           (when fragment "#")
           (when fragment
             (if* encode
                then (percent-encode-string
                      fragment
                      (if* *strict-parse*
                         then *fragment-bitvector-strict*
                         else *fragment-bitvector-non-strict*))
                else fragment))))))

    ;; calculate this cached slot
    (uri-parsed-path uri))

  res)

(defmethod iri-to-string ((iri iri))
  (uri-to-string iri))

(defmethod uri-to-string ((urn urn))
  ;; We can use render-uri here because no decoding/encoding happens for
  ;; URNs.
  (render-uri urn))

(defun render-parsed-path (path-list escape)
  (do* ((res '())
        (first (car path-list))
        (pl (cdr path-list) (cdr pl))
        (pe (car pl) (car pl)))
      ((null pl)
       (when res (apply #'concatenate 'string (nreverse res))))
    (when (or (null first)
              (prog1 (and (eq :absolute first)
                          ;; Only happens on Windows, in the case of a path
                          ;; with a drive letter in it.  The drive letter
                          ;; element is a keyword naming the drive.
                          (not (keywordp pe)))
                (setq first nil)))
      (push "/" res))
    (if* (symbolp pe)
       then ;; Only happens on Windows.  It's a keyword corresponding to
            ;; the drive letter.
            (push (format nil "~a:" pe) res)
     elseif (atom pe)
       then (if* escape
               then (push (percent-encode-string pe *pchar-bitvector*)
                          res)
               else (push pe res))
       else ;; contains params
            (if* escape
               then (push (percent-encode-string (car pe) *pchar-bitvector*)
                          res)
               else (push (car pe) res))
            (dolist (item (cdr pe))
              (push ";" res)
              (if* escape
                 then (push (percent-encode-string item *pchar-bitvector*)
                            res)
                 else (push item res))))))

(defmethod print-object ((uri uri) stream)
  (if* *print-escape*
     then (format stream "#<~a ~a>"
                  (class-name (class-of uri))
                  (render-uri uri))
     else (render-uri uri stream)))
