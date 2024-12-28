;;; mail.lisp --- CLI Mail Tools

;; 

;;; Code:
(in-package :cli/tools/mail)

(deferror mail-error (simple-error) () (:auto t))

(defvar *notmuch* (find-exe "notmuch"))
(defvar *offlineimap* (find-exe "offlineimap"))
(defvar *mail-program* :emacs)

(defun run-notmuch* (args &optional (output *standard-output*))
  (let ((proc (sb-ext:run-program *notmuch* (or (flatten args) nil) :output output)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (mail-error "NOTMUCH command failed: ~A ~A" *notmuch* (or args "")))))

(defun run-notmuch (&rest args)
  (run-notmuch* args))

(defun run-offlineimap* (args &optional (output *standard-output*))
  (let ((proc (sb-ext:run-program *offlineimap* (or (flatten args) nil) :output output)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (mail-error "OFFLINEIMAP command failed: ~A ~A" *offlineimap* (or args "")))))

(defun run-offlineimap (&rest args)
  (run-offlineimap* args))
