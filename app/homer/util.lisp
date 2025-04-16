;;; util.lisp --- Homer Utils

;; 

;;; Code:
(in-package :homer/core)

(defun homer-user-init ()
  (setq *user* (or (sb-posix:getenv "USER") "root")
        *user-homerc* (merge-homedir-pathnames ".homerc")
        *log-level* :info))

(defun mtime (path) (sb-posix:stat-mtime (sb-posix:stat path)))
(defun ctime (path) (sb-posix:stat-ctime (sb-posix:stat path)))

(defun compare-home-file (src)
  "Compare a SRC path to what is stored in the user's home. Return a cons with
the last modified timestamp of each file (SRC . HOME) or NIL."
  (let* ((name (enough-namestring src))
         (home (merge-pathnames name (user-homedir-pathname)))
         (m1 (mtime src))
         (m2 (when (probe-file home) (mtime home)))
         (status (cond
                   ((null m2) :new)
                   ((> m1 m2) :pull)
                   ((< m1 m2) (unless (= (ctime home) m2)
                                :push))
                   (t))))
    (cons status (cons src home))))

(defun homer-status (file)
  (let ((form (compare-home-file file)))
    (case (car form)
      ;; confirm with user
      (:new (println (format nil ":NEW ~A" (cdr form))))
      (:pull (println (format nil ":PULL ~A" (cadr form))))
      (:push (println (format nil ":PUSH ~A" (cddr form))))
      (t nil))))

(defun homer-copy (input output)
  (ensure-directories-exist output :verbose t)
  (uiop:copy-file input output))

(defun homer-maybe-push (file)
  (let ((form (compare-home-file file)))
    (case (car form)
      (:push (progn
               (println (format nil ":PUSH ~A" (cddr form)))
               (homer-copy (cddr form) (cadr form))))
      (t nil))))

(defun homer-maybe-pull (file)
  (let ((form (compare-home-file file)))
    (case (car form)
      (:pull (progn
               (println (format nil ":PULL ~A" (cddr form)))
               (homer-copy (cadr form) (cddr form))))
      (t nil))))

(defun homer-maybe-install (file)
  (let ((form (compare-home-file file)))
    (case (car form)
      (:pull (progn
               (println (format nil ":PULL ~A" (cddr form)))
               (homer-copy (cadr form) (cddr form))))
      (:new (progn
              (println (format nil ":NEW ~A" (cddr form)))
              (homer-copy (cadr form) (cddr form))))
      (:push (if *homer-force*
                 (progn
                   (println (format nil ":OVERWRITE ~A" (cddr form)))
                   (homer-copy (cadr form) (cddr form)))
                 (trace! "skipping file:" (cddr form))))
      (t nil))))

(defun home-config-slot (slot &optional (default :error))
  (let ((slot (find-symbol (string-upcase (string slot)) :homer/core)))
    (if (or (null *home-config*) (not (slot-boundp* *home-config* slot)))
        (if (eql default :error)
            (error "slot is unbound in homerc")
            default)
        (slot-value *home-config* slot))))
