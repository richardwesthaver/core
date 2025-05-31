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

;;  FIX 2025-04-18: takes a long time, do better
(defun music-metadata-scan (&optional (dir #l"mpk:media;music;"))
  (log:info! "walking music directory: ~A" dir)
  (walk-directory dir 
    (constantly t) ; collectp
    (constantly t) ; recursep
    (lambda (x) ; collector
      (dolist (y (directory-files x "*.*"))
        (when-let ((meta (ignore-errors (media-file-metadata y :list))))
          (setf (gethash y *music-metadata*) meta)))))
  *music-metadata*)

(defun get-music-metadata (k tag)
  (cdr (assoc tag (gethash k *music-metadata*) :test 'string-equal)))

(defun get-music-metadata* (tag)
  (let ((ret))
    (maphash-keys
     (lambda (k) (push (get-music-metadata k tag) ret))
     *music-metadata*)
    ret))

;; TODO 2025-04-30: 
(defun mpk-music-metadata-scan-parallel (&optional (dir #l"mpk:media;music;"))
  (with-task-pool (tp)
    (nyi!)))

;;  REVIEW 2025-04-18: good case for threading
#|
(time (mpk-music-metadata-scan)) ; 373.815s 
;; #<HASH-TABLE :TEST EQL :COUNT 62513 {1096798293}>
(time ;; unique tags (case-insensitive)
 (reduce (lambda (a b) (union a b :test 'string-equal))
  (let ((ret)) 
   (maphash (lambda (k v) (push (mapcar 'car v) ret)) 
    mpk:*music-metadata*) ret))) ; 5.9s
(length *)
;; 274
|#
