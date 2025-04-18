;;; util.lisp --- MPK Utilities

;; 

;;; Code:
(in-package :mpk)

(defun mpk-media-collection (k)
  (gethash k *mpk-media-collections*))

(defun mpk-user-path (path)
  (merge-pathnames path *mpk-user-directory*))

(defun mpk-media-path (path)
  (merge-pathnames path *mpk-media-directory*))

(defun mpk-music-path (path)
  (merge-pathnames path (mpk-media-collection :music)))

(defun mpk-data-path (path)
  (merge-pathnames path *mpk-data-directory*))

(defun mpk-ensure-directories ()
  (maphash-values (lambda (p) (ensure-directories-exist p :verbose t)) *mpk-media-collections*)
  (ensure-directories-exist *mpk-user-directory* :verbose t))

;;  FIX 2025-04-18: takes a long time, do better
(defun mpk-music-metadata-scan (&optional (dir (directory-path (mpk-media-collection :music))))
  (log:info! "walking music directory: ~A" dir)
  (setq *music-metadata* nil)
  (walk-directory dir 
    (constantly t) ; collectp
    (constantly t) ; recursep
    (lambda (x) ; collector
      (dolist (y (directory-files x "*.*"))
        (when-let ((meta (ignore-errors (media-file-metadata y :list))))
          (push meta *music-metadata*))))))


;;  REVIEW 2025-04-18: good case for preduce
;; (reduce 'union 

;; (reduce 'union (mapcar (lambda (x) (mapcar 'car x)) *music-metadata*))

                    
