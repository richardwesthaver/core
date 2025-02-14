;;; mail.lisp --- CLI Mail Tools

;; 

;;; Code:
(in-package :cli/tools/mail)

(deferror mail-error (simple-error) () (:auto t))

(defvar *notmuch* (find-exe "notmuch"))
(defvar *offlineimap* (find-exe "offlineimap"))
(defvar *mail-program* :emacs)

(defun %notmuch (args output &optional (wait t))
  (sb-ext:run-program *notmuch* (or (flatten args) nil) :output output :wait wait))

(defun run-notmuch* (args &optional (output *standard-output*))
  (let ((proc (%notmuch args output)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (mail-error "NOTMUCH command failed: ~A ~A" *notmuch* (or args "")))))

(defun run-notmuch (&rest args)
  (run-notmuch* args))

(defun run-offlineimap* (args &optional (output *standard-output*) wait)
  (let ((proc (sb-ext:run-program *offlineimap* (or (flatten args) nil) :output output :wait wait)))
    (when wait
      (if (eq 0 (sb-ext:process-exit-code proc))
          nil
          (mail-error "OFFLINEIMAP command failed: ~A ~A" *offlineimap* (or args ""))))))

(defun run-offlineimap (&optional wait args)
  (run-offlineimap* args *standard-output* wait))

(defun notmuch-search (query)
  (let* ((proc (%notmuch `("search" "--format=sexp" ,query) :stream nil))
         (out (sb-ext:process-output proc)))
    (read out)))

(defun notmuch-show (query)
  (let* ((proc (%notmuch `("show" "--format=sexp" ,query) :stream nil))
         (out (sb-ext:process-output proc)))
    (read out)))

(defun notmuch-count (query)
  (let* ((proc (%notmuch `("count" ,query) :stream nil))
         (out (sb-ext:process-output proc)))
    (read out)))

(defun notmuch-tag (query)
  (run-notmuch "tag" query))

(defun notmuch-address (query)
  (let* ((proc (%notmuch `("address" "--format=sexp" ,query) :stream nil))
         (out (sb-ext:process-output proc)))
    (read out)))
