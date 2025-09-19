;;; obj/uri/intern.lisp --- Support for URI interning

;; support for interning URIs

;;; Code:
(in-package :obj/uri)

(defmethod uri= ((uri1 uri) (uri2 uri))
  (when (not (eq (uri-scheme uri1) (uri-scheme uri2)))
    (return-from uri= nil))
  ;; RFC2396 says: a URL with an explicit ":port", where the port is
  ;; the default for the scheme, is the equivalent to one where the
  ;; port is elided.  Hmmmm.  This means that this function has to be
  ;; scheme dependent.  Grrrr.
  (let ((default-port (case (uri-scheme uri1)
                        (:http 80)
                        (:https 443)
                        (:ftp 21)
                        (:telnet 23))))
    (and (equalp (uri-host uri1) (uri-host uri2))
         (equalp (uri-userinfo uri1) (uri-userinfo uri2))
         (eql (or (uri-port uri1) default-port)
              (or (uri-port uri2) default-port))
         (string= (uri-path uri1) (uri-path uri2))
         (string= (uri-query uri1) (uri-query uri2))
         (string= (uri-fragment uri1) (uri-fragment uri2)))))

(defmethod uri= ((urn1 urn) (urn2 urn))
  (when (not (eq (uri-scheme urn1) (uri-scheme urn2)))
    (return-from uri= nil))
  (and (equalp (urn-nid urn1) (urn-nid urn2))
       (urn-nss-equal (urn-nss urn1) (urn-nss urn2))))

(defun make-uri-space (&rest keys &key (size 777) &allow-other-keys)
  (apply 'std/hash:make-hashset size #'uri= #'uri-hash keys))

(defun uri-hash (uri)
  (if* (uri-hashcode uri)
     thenret
     else (setf (uri-hashcode uri) (sxhash (render-uri uri nil)))))

(defvar *uris* (make-uri-space))

(defun uri-space () *uris*)

(defun (setf uri-space) (new-val)
  (setq *uris* new-val))

(defun urn-nss-equal (nss1 nss2 &aux len)
  ;; Return t iff the nss values are the same.
  ;; %2c and %2C are equivalent.
  (when (or (null nss1) (null nss2)
            (not (= (setq len (length nss1))
                    (length nss2))))
    (return-from urn-nss-equal nil))
  (do* ((i 0 (1+ i))
        (state :char)
        c1 c2)
       ((= i len) t)
    (setq c1 (schar nss1 i))
    (setq c2 (schar nss2 i))
    (ecase state
      (:char
       (if* (and (char= #\% c1) (char= #\% c2))
          then (setq state :percent+1)
        elseif (char/= c1 c2)
          then (return nil)))
      (:percent+1
       (when (char-not-equal c1 c2) (return nil))
       (setq state :percent+2))
      (:percent+2
       (when (char-not-equal c1 c2) (return nil))
       (setq state :char)))))

(defmethod intern-uri ((xuri uri) &optional (uri-space *uris*))
  (let ((uri (hashset-find uri-space xuri)))
    (if* uri
       thenret
       else (hashset-insert uri-space xuri))))

(defmethod intern-uri ((uri string) &optional (uri-space *uris*))
  (intern-uri (parse-uri uri) uri-space))

(defun unintern-uri (uri &optional (uri-space *uris*))
  (if* (eq t uri)
     then (clrhash uri-space)
   elseif (uri-p uri)
     then (hashset-remove uri-space uri)
     else (error "bad uri: ~s." uri)))

(defmacro do-all-uris ((var &optional uri-space result-form)
		       &body body)
  "do-all-uris (var [[uri-space] result-form])
  		    {declaration}* {tag | statement}*
Executes the forms once for each uri with var bound to the current uri"
  (let ((f (gensym))
	(g-uri-space (gensym)))
    `(let ((,g-uri-space (or ,uri-space *uris*)))
       (prog nil
	  (flet ((,f (,var)
		   (declare (ignorable ,var))
		   (tagbody ,@body)))
	    (map-hashset #',f ,g-uri-space))
	  (return ,result-form)))))
