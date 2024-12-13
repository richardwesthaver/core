;;; net.lisp --- Net Tools

;; 

;;; Code:
(in-package :cli/tools/net)

;;; Browser
(deferror simple-browser-error (simple-error) () (:auto t))

(defparameter *browser* (or (find-exe "chromium") (find-exe "firefox")))

(defun run-browser (&rest args)
  (let ((proc (sb-ext:run-program *browser* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (simple-browser-error "browser command failed: ~A ~A" args))))

(defun browse-url (url)
  (run-browser (render-uri url)))

;;; IP
(deferror simple-ip-error (simple-error) () (:auto t))

(defparameter *ip* (find-exe "ip"))

(defun run-ip (&rest args)
  (let ((proc (sb-ext:run-program *ip* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (simple-ip-error "ip command failed: ~A ~A" args))))

(defun ip-link-add (dev &optional (type "wireguard"))
  (run-ip "link" "add" "dev" dev "type" type))

(defun ip-link-up (dev)
  (run-ip "link" "set" "up" "dev" dev))

(defun ip-addr-add (dev addr &optional peer)
  (apply 'run-ip "address" "add" "dev" dev addr (when peer (list "peer" peer))))

;;; Wireguard
(deferror wg-error (simple-error error) () (:auto t))

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

(defun wg-setconf (dev conf)
  (run-wg "setconf" dev conf))

(defun wg-set (dev &key listen-port private-key peer allowed-ips endpoint)
  (let ((args (list "set" dev)))
    (when listen-port (appendf args (list listen-port "listen-port")))
    (when private-key (appendf args (list private-key "private-key")))
    (when peer (appendf args (list peer "peer")))
    (when allowed-ips (appendf args (list allowed-ips "allowed-ips")))
    (when endpoint (appendf args (list endpoint "endpoint")))
    (apply 'run-wg args)))

(defun wg-show (dev)
  (run-wg "show" dev))

(defun wg-showconf (conf)
  (run-wg "showconf" conf))

;;; YTDL
(deferror ytdl-error (simple-error error) () (:auto t))

(defvar *ytdl* (or (find-exe "yt-dlp")
                   (find-exe "youtube-dl")))

(defun run-ytdl* (args &optional (output *standard-output*) input)
  (let ((proc (if input
                  (sb-ext:run-program *ytdl* (or args nil) :output :stream :input input)
                  (sb-ext:run-program *ytdl* (or args nil) :output :stream))))
  (with-open-stream (s (sb-ext:process-output proc))
    (loop for l = (read-line s nil nil)
          while l
          do (write-string l output)))
  (if (eq 0 (sb-ext:process-exit-code proc))
      nil
      (ytdl-error "YTDL command failed: ~A ~A" *ytdl* (or args "")))))

(defun run-ytdl (&rest args)
  (run-ytdl* args))
