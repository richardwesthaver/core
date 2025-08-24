;;; util.lisp --- MPK Utilities

;; 

;;; Code:
(in-package :mpk)
(in-readtable :std)

(defun find-mpk-symbol (s) (find-symbol* (symbol-name s) :mpk nil))

(defun mpk-media-collection (k)
  (gethash k *mpk-media-collections*))

(defun mpk-user-path (path)
  (merge-pathnames path *mpk-user-directory*))

(defun mpk-media-path (path)
  (merge-pathnames path *mpk-media-directory*))

(defun mpk-cache-path (path)
  (merge-pathnames path *mpk-cache-directory*))

(defun mpk-music-path (path)
  (merge-pathnames path (mpk-media-collection :music)))

(defun mpk-data-path (path)
  (merge-pathnames path *mpk-data-directory*))

(defun mpk-db-path (path)
  (merge-pathnames path *mpk-db-directory*))

(defun mpk-ensure-directories ()
  (values
   (maphash-values (lambda (p) (ensure-directories-exist p :verbose t)) *mpk-media-collections*)
   (ensure-directories-exist *mpk-user-directory* :verbose t)
   (ensure-directories-exist *mpk-data-directory* :verbose t)
   (ensure-directories-exist *mpk-cache-directory* :verbose t)))

(defvar *music-metadata* (make-hash-table :test 'equal :synchronized t))

(defun metadata-scan-directory (&optional (dir #l"mpk:media;music;") (table *music-metadata*))
  (log:info! "walking music directory: ~A" dir)
    (walk-directory dir 
      (constantly t) ; collectp
      (constantly t) ; recursep
      (lambda (x) ; collector
        (dolist (y (directory-files x "*.*"))
          (when-let ((meta (ignore-errors (media-file-metadata y :list)))
                     (y y))
            ;; (appendf meta (cons 'hash (cry/b3:b3sum y)))
            (setf (gethash y table) meta)))))
  table)

(defun get-music-metadata (k tag)
  (cdr (assoc tag (gethash k *music-metadata*) :test 'string-equal)))

(defun get-music-metadata* (tag)
  (let ((ret))
    (maphash-keys
     (lambda (k) (push (get-music-metadata k tag) ret))
     *music-metadata*)
    ret))

(defvar *music-metadata-tags* nil)

(defun normalize-metadata-tag (str)
  "Normalize a metadata tag."
  (declare (simple-string str))
  (substitute #\- #\space (substitute #\- #\_ (string-downcase str))))

(defun music-metadata-tags ()
  (maphash-values
   (lambda (x) 
     (mapc (lambda (y) 
             (pushnew (normalize-metadata-tag (car y)) *music-metadata-tags* :test 'string-equal)) 
           x))
   *music-metadata*)
  (setf *music-metadata-tags* (sort *music-metadata-tags* 'string<)))

(defun ab-hi-tags (&optional (list *music-metadata-tags*))
  (let (ret)
    (dolist (tag list ret)
      (when (starts-with-p tag "ab:hi:")
        (push (subseq tag 6) ret)))))

(defun mpk-music-metadata-scan-parallel (&optional (dir #l"mpk:media;music;") (table *music-metadata*))
  (with-submit-counted
    (walk-directory dir 
      (constantly t) ; collectp
      (constantly t) ; recursep
      (lambda (x) ; collector
        (submit-counted
         (lambda ()
           (dolist (y (directory-files x "*.*"))
             (when-let ((meta (ignore-errors (media-file-metadata y :list)))
                        (y y))
               (log:info! "adding metadata for ~A" y)
               ;; (appendf meta (cons 'hash (cry/b3:b3sum y)))
               (setf (gethash y table) meta)))))))
    (receive-counted)))

;;  REVIEW 2025-04-18: good case for threading
#|
(time ;; unique tags (case-insensitive)
 (reduce (lambda (a b) (union a b :test 'string-equal))
  (let ((ret)) 
   (maphash (lambda (k v) (push (mapcar 'car v) ret)) 
    mpk:*music-metadata*) ret))) ; 5.9s
(length *)
;; 274
|#
