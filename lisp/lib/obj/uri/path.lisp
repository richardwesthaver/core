;;; obj/uri/path.lisp --- URI Path merging functions

;;

;;; Code:
(in-package :obj/uri)
;; merging and unmerging
(defmethod merge-uris ((uri string) (base string) &optional place)
  (merge-uris (parse-uri uri) (parse-uri base) place))

(defmethod merge-uris ((uri uri) (base string) &optional place)
  (merge-uris uri (parse-uri base) place))

(defmethod merge-uris ((uri string) (base uri) &optional place)
  (merge-uris (parse-uri uri) base place))

(defmethod merge-uris ((uri uri) (base uri) &optional place)
  ;; When PLACE is nil, this function returns a new URI.
  ;; When PLACE is non-nil, it is return.
  (tagbody
    (when (and (null (uri-path uri))
               (null (uri-scheme uri))
               (null (uri-host uri))
               (null (uri-userinfo uri))
               (null (uri-port uri))
               (null (uri-query uri)))
      (return-from merge-uris
        (let ((new (copy-uri base :place place)))
          (when (uri-query uri)
            (setf (uri-query new) (uri-query uri)))
          (when (uri-fragment uri)
            (setf (uri-fragment new) (uri-fragment uri)))
          new)))

    (setq uri (copy-uri uri :place place))

    (when (uri-scheme uri) (go :done))

    (setf (uri-scheme uri) (uri-scheme base))

    ;; if URI has a host, we're done
    (when (uri-host uri) (go :done))

    (set-host uri
              (%uri-host base)
              (%uri-ipv6 base)
              (%uri-zone-id base))
    (setf (uri-userinfo uri) (uri-userinfo base))
    (setf (uri-port uri) (uri-port base))

    (let ((p (uri-parsed-path uri)))
      (when (null p)
        (setf (uri-path uri) (uri-path base))
        (go :done))

      (when (and p (eq :absolute (car p)))
        (if* (equal '(:absolute "") p)
           then ;; Canonicalize the way parsing does:
                (setf (uri-path uri) nil)
         elseif (eq :absolute (first p))
           then ;; this also sets uri-path
                (multiple-value-bind (new changed)
                    (canonicalize-path-list p)
                  (when changed
                    (setf (uri-parsed-path uri) new))))
        (go :done)))

    (let* ((base-path
            (or (uri-parsed-path base)
                ;; needed because we canonicalize away a path of just `/':
                '(:absolute "")))
           (path (uri-parsed-path uri))
           new-path-list)
      (when (not (eq :absolute (car base-path)))
        (error "Cannot merge ~a and ~a, since the latter is not absolute."
               uri base))

      (setq new-path-list
        (append (butlast base-path)
                (if* path then (cdr path) else '(""))))

      (let ((last (last new-path-list)))
        (if* (atom (car last))
           then (when (string= "." (car last))
                  (setf (car last) ""))
           else (when (string= "." (caar last))
                  (setf (caar last) ""))))
      (setq new-path-list
        (delete "." new-path-list :test #'(lambda (a b)
                                            (if* (atom b)
                                               then (string= a b)
                                               else nil))))

      (let ((npl (cdr new-path-list))
            index tmp fix-tail)
        (setq fix-tail
          (string= ".." (let ((l (car (last npl))))
                          (if* (atom l)
                             then l
                             else (car l)))))
        (loop
          (setq index
            (position ".." npl
                      :test #'(lambda (a b)
                                (string= a
                                         (if* (atom b)
                                            then b
                                            else (car b))))))
          (when (null index) (return))

          (if* (= 0 index)
             then ;; rfe11852: RFC 3986, in section 5.4.2 (Abnormal
                  ;; Examples) says parsers; must be careful in handling
                  ;; cases where there are more ".." segments in a
                  ;; relative-path reference than there are in the base
                  ;; URI's path.  The examples, between the two RFC's were
                  ;; changed to show the additional, leading ..'s to be
                  ;; removed. So, we'll do that now.
                  (setq npl (cdr npl))
           elseif (= 1 index)
             then (setq npl (cddr npl))
             else (setq tmp npl)
                  (dotimes (x (- index 2)) (setq tmp (cdr tmp)))
                  (setf (cdr tmp) (cdddr tmp))))
        (setf (cdr new-path-list) npl)
        (when fix-tail (setq new-path-list (nconc new-path-list '("")))))

      (when (eq :absolute (first new-path-list))
        (multiple-value-bind (new changed)
            (canonicalize-path-list new-path-list)
          (when changed (setq new-path-list new))))

      ;; Also sets uri-path:
      (setf (uri-parsed-path uri) new-path-list))

   :done
    (return-from merge-uris uri)))

(defun canonicalize-path-list (path-list &aux changed)
  ;; Return two values: new version of PATH-LIST and an indicator if it was
  ;; changed.  We are only called when (car path-list) is :absolute.
  (loop while (or (equal "." (second path-list))
                  (equal ".." (second path-list)))
        do (setf (cdr path-list) (cddr path-list))
           (setq changed t))
  (values path-list changed))

(defmethod merge-uris ((urn urn) (base urn) &optional place)
  (if* place
     then (setf (urn-nid place) (urn-nid urn))
          (setf (urn-nss place) (urn-nss urn))
          place
     else urn))

(defmethod merge-uris ((urn urn) (base uri) &optional place)
  (if* place
     then (setf (urn-nid place) (urn-nid urn))
          (setf (urn-nss place) (urn-nss urn))
          place
     else urn))

(defmethod merge-uris ((uri uri) (base urn) &optional place)
  (copy-uri uri :place place))

(defmethod enough-uri ((uri string) (base string) &optional place)
  (enough-uri (parse-uri uri) (parse-uri base) place))

(defmethod enough-uri ((uri uri) (base string) &optional place)
  (enough-uri uri (parse-uri base) place))

(defmethod enough-uri ((uri string) (base uri) &optional place)
  (enough-uri (parse-uri uri) base place))

(defmethod enough-uri ((uri uri) (base uri) &optional place)
  ;; Like ENOUGH-PATHNAME, but for URIs.
  (let ((new-scheme nil)
        (new-host nil)
        (new-ipv6 nil)
        (new-zone-id nil)
        (new-userinfo nil)
        (new-port nil)
        (new-parsed-path nil))

    ;; If the scheme and authority are not the same, then return URI.
    (when (or (and (uri-scheme uri)
                   (not (equalp (uri-scheme uri) (uri-scheme base))))
              ;; We don't use uri-authority, because it conses a lot.
              (and (uri-host uri)
                   (not (equalp (uri-host uri) (uri-host base))))
              (not (equalp (uri-userinfo uri) (uri-userinfo base)))
              (not (equalp (uri-port uri) (uri-port base))))
      (return-from enough-uri uri))

    ;; For this group, if the slot is nil in URI, then the return value is
    ;; copied from from BASE:
    (when (null (uri-scheme uri)) (setq new-scheme (uri-scheme base)))
    (when (null (uri-host uri))
      ;; These are copied as a unit:
      (setq new-host (%uri-host base))
      (setq new-ipv6 (%uri-ipv6 base))
      (setq new-zone-id (%uri-zone-id base)))
    (when (null (uri-userinfo uri)) (setq new-userinfo (uri-userinfo base)))
    (when (null (uri-port uri)) (setq new-port (uri-port base)))

    ;; Now, for the hard one, path.
    ;; We essentially do here what enough-namestring does.
    (do* ((base-path (uri-parsed-path base))
          (path (uri-parsed-path uri))
          (bp base-path (cdr bp))
          (p path (cdr p)))
        ((or (null bp) (null p))
         ;; If p is nil, that means we have something like
         ;; (enough-uri "/foo/bar" "/foo/bar/baz.htm"), so
         ;; new-parsed-path will be nil.
         (when (null bp)
           (setq new-parsed-path (copy-list p))
           (when (not (symbolp (car new-parsed-path)))
             (push :relative new-parsed-path))))
      (if* (equal (car bp) (car p))
         thenret ;; skip it
         else (setq new-parsed-path (copy-list p))
              (when (not (symbolp (car new-parsed-path)))
                (push :relative new-parsed-path))
              (return)))

    (let ((new-path 
           (or (when new-parsed-path
                 (render-parsed-path new-parsed-path
                                     ;; don't know, so have to assume:
                                     t))
               ;; can't have a completely empty uri!
               "/")))
      (copy-uri nil :class (class-of uri) :place place
            ;;; these come from base if the original slot was nil
                :scheme new-scheme
                :host new-host
                :ipv6 new-ipv6
                :zone-id new-zone-id
                :userinfo new-userinfo
                :port new-port
                :path new-path
                :parsed-path new-parsed-path
            ;;; never from base... why? is this documented?
                :query (uri-query uri)
                :fragment (uri-fragment uri)
                :plist (copy-list (uri-plist uri))))))

(defmethod enough-uri ((urn urn) (base urn) &optional place)
  (if* place
     then (setf (urn-nid place) (urn-nid urn))
          (setf (urn-nss place) (urn-nss urn))
          place
     else urn))

(defmethod enough-uri ((urn urn) (base uri) &optional place)
  (declare (ignore place))
  (error "enough-uri of a URN (~a) and URI (~a)." urn base))

(defmethod enough-uri ((uri uri) (base urn) &optional place)
  (declare (ignore place))
  (error "enough-uri of a URI (~a) and URN (~a)." uri base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uri-to-pathname (uri)
  ;; On Windows, turn file:///d:/foo/bar.cl into #p"d:/foo/bar.cl"
  ;; On UNIX,    turn file:///foo/bar.cl    into #p"/foo/bar.cl"
  (when (not (eq :file (uri-scheme uri)))
    (error "Only file: URIs can be converted to pathnames: ~s." uri))
  (when (null (uri-path uri)) (error "URI has no path: ~s." uri))
    (pathname
     (percent-decode-string
      (uri-path uri)
      nil)))

(defun pathname-to-uri (pathname)
  (when (not (uiop:absolute-pathname-p pathname t))
    (error "A relative pathname cannot be converted to a URI: ~s." pathname))
  (parse-uri
   (let ((s (percent-encode-string
             #+mswindows (substitute #\/ #\\ (namestring pathname))
             #-mswindows (namestring pathname)
             *pchar/-bitvector*)))
     #-mswindows (format nil "file://~a" s)
     #+mswindows (if* (pathname-device pathname)
                    then (format nil "file:///~a" s)
                    else (format nil "file://~a" s)))))
