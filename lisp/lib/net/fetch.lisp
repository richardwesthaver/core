(in-package :net/fetch)

(define-condition invalid-path-error (error)
  ((text :initarg :text :reader text)))

(defun download (url &optional output)
  (let ((output (if output
                    output
                    (file-namestring (obj/uri:uri-path (obj/uri:uri url))))))
    (multiple-value-bind (stream status header uri)
        (req:get url :want-stream t :keep-alive nil :use-connection-pool t)
      (when (= status 200) (write-stream-into-file stream (pathname output)))
      (values (or stream uri header)
              status))))

(defun split-file-path (path)
  (let ((pos-last-slash (1+ (position #\/ path :from-end t))))
    (list (subseq path 0 pos-last-slash)
          (subseq path pos-last-slash))))

(defun split-uri-string (uri-string)
  (let ((pu (parse-uri uri-string)))
    (cons (uri-host pu) (split-file-path (uri-path pu)))))

(defun condition-path (path)
  "Abuse parse-uri to strip possible get args from path"
  (let ((p (parse-uri path))) (uri-path p)))

(defun is-file (path)
  (handler-case (probe-file path)
    (type-error (e) #+sbcl (declare (ignore e)) (error 'invalid-path-error
                                                       :text (format nil "Invalid path: ~A" path)))))

(defun %fetch (url-or-path &key (cache t)
                                dir
                                (flush nil))
  (cond
    ((is-file (condition-path url-or-path)) (condition-path url-or-path))
    ((is-file (condition-path (concatenate 'string  dir url-or-path)))
     (condition-path (concatenate 'string  dir url-or-path)))
    ((parse-uri url-or-path)
     (let* ((tmp-pathname (split-uri-string url-or-path))
            (file-pathstring (format nil "~{~A~^~}" (if dir (cons dir tmp-pathname) tmp-pathname)))
            (file-pathname (ensure-directories-exist
                            file-pathstring)))
       (if flush
           (when (is-file file-pathname) (delete-file file-pathname))
           (if (and cache (probe-file file-pathname))
               (values file-pathname 200 "OK")
               (download url-or-path file-pathname)))))
    (t (values nil 404 "Not file of url"))))

(defun fetch (url-or-path
              &key
              (dir)
              (external-format :utf-8)
              (cache t)
              (stream nil)
              (flush nil))
  "Fetch file from ~url-or-location~ if not cached in ~dir~
stores the file in the location specified by dir if url or file is url the file
is stored in ~dir~/~uri-host~/~uri-path~.

Note that it is important to ensure that dir and subdir if used end in a /

-return: path to file or stream if :stream parameter is passed
-arguments:
  - url-or-path: <string> pathname or url string identifying file to be fetched.
  - stream: resuests that fetch returns a stream
  - cache: <T|NIL> if T looks for file in -dir and uses that as source if NIL then the a fresh copy of the file is fetched
  - dir: location to store fetched file.
  - flush: if T fetch does not download the file it deletes the existing file.
"
  (let ((fetched-path (%fetch url-or-path :dir dir :cache cache :flush flush)))
    (if (not fetched-path)
        nil
        (if stream
            (open fetched-path :direction :input :external-format external-format)
            fetched-path))))
