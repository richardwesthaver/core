;;; wg.lisp --- WireGuard Tools

;; CLI Access to wg* tools from lisp. Requires the wireguard package.

;;; Code:
(in-package :cli/tools/wg)
(deferror cc-error (simple-error error) ())

(defun wg-error (fmt &rest args)
  (error 'wg-error :format-arguments args :format-control fmt))

(defparameter *wg* (find-exe "wg"))

(defun run-wg* (args &optional (output *standard-output*) input)
  (let ((proc (if input
                  (sb-ext:run-program *wg* (or args nil) :output :stream :input input)
                  (sb-ext:run-program *wg* (or args nil) :output :stream))))
  (with-open-stream (s (sb-ext:process-output proc))
    (loop for l = (read-line s nil nil)
          while l
          do (write-string l  output)))
  (if (eq 0 (sb-ext:process-exit-code proc))
      nil
      (wg-error "WG command failed: ~A ~A" *wg* (or args "")))))

(defun run-wg (&rest args)
  (run-wg* args))

(defun wg-private-key ()
  (with-output-to-string (s)
    (run-wg* '("genkey") s)))

(defun wg-public-key (private-key)
  (with-output-to-string (public-key)
    (with-input-from-string (s private-key)
      (run-wg* '("pubkey") public-key s))))

(defun wg-generate-keys ()
  "Generate a wireguard keypair, returning (values PUBLIC-KEY PRIVATE-KEY)."
  (let* ((privkey (wg-private-key))
         (pubkey (wg-public-key privkey)))
    (values pubkey privkey)))


(defun wg-generate-key-files (&optional (private "private.key") (public "public.key"))
  (multiple-value-bind (pubkey privkey) (wg-generate-keys)
    (with-umask #o077
      (log:trace! "setting umask to 077")
      (with-open-file (f public :direction :output)
        (write-line pubkey f))
      (with-open-file (f private :direction :output)
        (write-line privkey f)))))

