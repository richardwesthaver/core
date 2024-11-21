;;; net/fetch.lisp --- Simple HTTP Downloads

;; Provides the DOWNLOAD and FETCH functions for easily download remote files.

;;; Commentary:

;;; Code:
(in-package :net/fetch)

(define-condition invalid-path-error (error)
  ((text :initarg :text :reader text)))

(defvar *default-fetch-output-file* #P"index.html")

(defun download (url &key (output (or (obj/uri:uri-path (obj/uri:uri url)) *default-fetch-output-file*))
                          (if-exists :error) (progress nil) (connect-timeout net/req:*default-connect-timeout*)
                          cookies)
  "Download a file from URL to OUTPUT."
  (let ((*progress-bar-enabled* progress))
    (multiple-value-bind (stream status header uri)
        (req:get url 
                 :want-stream t 
                 :force-binary t
                 :connect-timeout connect-timeout
                 :verbose (log:trace-p)
                 :cookie-jar cookies)
      (when (= status 200)
        (log:debug! "download connect OK:" url)
        (log:debug! "headers:" (hash-table-alist header))
        (let ((len (gethash "content-length" header))
              (buff (make-array 4096 :element-type 'octet :adjustable t)))
          (when len (setf len (parse-integer len)))
          (with-progress-maybe progress (len "downloading ~a to ~a..." url output)
            (with-open-file (out output :direction :output :element-type 'octet :if-exists if-exists :if-does-not-exist :create)
              (loop
                (let ((end (read-sequence buff stream :end 4096)))
                  (when progress (update-progress *progress-bar* end))
                  (write-sequence buff out :end end)
                  (unless (= end 4096)
                    (return))))))))
      (values status header uri))))

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
    ((is-file (condition-path (concatenate 'string  dir (uri-path url-or-path))))
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
               (download url-or-path :output file-pathname)))))
    (t (values nil 404 "Not file of url"))))

(defun fetch (url
              &key
              (dir)
              (external-format :utf-8)
              (cache t)
              (stream nil)
              (flush nil))
  "Fetch file from URL if not cached in DIR
stores the file in the location specified by dir if url or file is url the file
is stored in DIR/URI-HOST/URI-PATH.

Note that it is important to ensure that dir and subdir if used end in a /

-return: path to file or stream if :stream parameter is passed
-arguments:
  - url: <string> pathname or url string identifying file to be fetched.
  - stream: resuests that fetch returns a stream
  - cache: <T|NIL> if T looks for file in -dir and uses that as source if NIL then the a fresh copy of the file is fetched
  - dir: location to store fetched file.
  - flush: if T fetch does not download the file it deletes the existing file.
"
  (let ((fetched-path (%fetch url :dir dir :cache cache :flush flush)))
    (if (not fetched-path)
        nil
        (if stream
            (open fetched-path :direction :input :external-format external-format)
            fetched-path))))
