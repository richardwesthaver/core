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

(defconfig browser-config (ast) ())

(defconfig chromium-config (browser-config) ())

(defmethod make-config ((obj (eql :chromium)) &key ast)
  (make-instance 'chromium-config :ast ast))

(defconfig firefox-config (browser-config) ())

(defmethod make-config ((obj (eql :firefox)) &key ast)
  (make-instance 'firefox-config :ast ast))

;;; IP
(define-cli-tool :ip (&rest args)
  (let ((proc (sb-ext:run-program *ip* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (ip-error "ip command failed: ~A ~A" args))))

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

(when *wg* (pushnew :wg *cli-tools*))

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

;;; NMAP
(deferror nmap-error (simple-error error) () (:auto t))

(defvar *nmap* (find-exe "nmap"))

(defun run-nmap* (args &optional (output *standard-output*) input)
  (let ((proc (if input
                  (sb-ext:run-program *nmap* (or args nil) :output output :input input)
                  (sb-ext:run-program *nmap* (or args nil) :output output))))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (nmap-error "NMAP command failed: ~A ~A" *nmap* (or args "")))))

(defun run-nmap (&rest args)
  (run-nmap* args))

(when *nmap* (pushnew :nmap *cli-tools*))
  

;;; YTDL
;; ref: https://github.com/yt-dlp/yt-dlp
(deferror ytdl-error (simple-error error) () (:auto t))

(defvar *ytdl* (or (find-exe "yt-dlp")
                   (find-exe "youtube-dl")))

(defmacro with-ytdl ((args &optional output proc input) &body body)
  (with-gensyms (s)
    `(let ((,(or proc s)
           (if ,input
	       (sb-ext:run-program *ytdl* ,(or args nil) :output ,output :input ,input)
	       (sb-ext:run-program *ytdl* ,(or args nil) :output ,output))))
       (unwind-protect (progn ,@body)
         (unless (eq 0 (sb-ext:process-exit-code ,(or proc s)))
	   nil
	   (ytdl-error "YTDL command failed: ~A ~A" *ytdl* ,(or args "")))))))
  
(defun run-ytdl (&rest args)
  (with-ytdl (args *standard-output*)))

(defun ytdl-extractors ()
  "Return the list of available YTDL extractors."
  (mapcar 
   'trim
   (lines
    (with-output-to-string (s)
      (with-ytdl ((list "--list-extractors") s))))))

(defun ytdl-user-agent ()
  "Return the current YTDL user-agent."
  (trim
   (with-output-to-string (s)
     (with-ytdl ((list "--dump-user-agent") s)))))

(defun ytdl-list (playlist)
  "Return a list of matches for given PLAYLIST."
  (mapcar
   'trim
   (lines
    (with-output-to-string (s)
      (with-ytdl (`("--flat-playlist" "--print" "id" ,playlist) s))))))

(defun ytdl-json (query)
  "Return the infojson for a given track or playlist QUERY."
  (deserialize
   (with-output-to-string (s)
     (with-ytdl (`("--dump-json" ,query) s)))
   :json))
  
;;; Caddy
(deferror caddy-error (simple-error error) () (:auto t))

(defvar *caddy* (find-exe "caddy"))

(defun run-caddy* (args &optional (output *standard-output*))
  (let ((proc (sb-ext:run-program *caddy* (or (flatten args) nil) :output output)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (caddy-error "CADDY command failed: ~A ~A" *caddy* (or args "")))))

(defun run-caddy (&rest args)
  (run-caddy* args))

(defun start-caddy (&rest args)
  (apply 'run-caddy "start" args))

(when *caddy* (pushnew :caddy *cli-tools*))

#|
(start-caddy)

(req:post "http://127.0.0.1:2019/load" :headers '(("Content-Type" . "application/json")) :content "    {
\"apps\": {
   \"http\": {
       \"servers\": {
           \"hello\": {
               \"listen\": [\":2015\"],
               \"routes\": [
                   {
                       \"handle\": [{
                           \"handler\": \"static_response\",
                           \"body\": \"Hello, world!\"
                       }]
                   }
               ]
           }
       }
   }
}
}")

;; OK

(req:get "http://127.0.0.1:2015") ;; Hello, world!
|#

;;; Transmission

#| env
TR_APP_VERSION ; Transmission's short version string, e.g. 4.0.0
TR_TIME_LOCALTIME
TR_TORRENT_BYTES_DOWNLOADED ; Number of bytes that were downloaded for this torrent
TR_TORRENT_DIR ; Location of the downloaded data
TR_TORRENT_HASH ; The torrent's info hash
TR_TORRENT_ID
TR_TORRENT_LABELS ; A comma-delimited list of the torrent's labels
TR_TORRENT_NAME ; Name of torrent (not filename)
TR_TORRENT_PRIORITY ; The priority of the torrent (Low is "-1", Normal is "0", High is "1")
TR_TORRENT_TRACKERS ; A comma-delimited list of the torrent's trackers' announce URLs
|#

(defvar *transmission-user-config-directory* (merge-homedir-pathnames ".config/transmission/"))

(defconfig transmission-config ()
  ((settings :initarg :settings :type transmission-settings)))

(defconfig transmission-settings (json:json-object) ())

(defmethod make-config ((obj (eql :transmission)) &key settings)
  (make-instance 'transmission-config :settings settings))

(defun load-transmission-config (&optional (path *transmission-user-config-directory*))
  (make-config :transmission 
               :settings (change-class 
                          (deserialize (merge-pathnames "settings.json" path) :json)
                          'transmission-settings)))

(define-cli-tool :transmission-remote (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *transmission-remote* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (transmission-remote-error "TRANSMISSION-REMOTE command failed: ~A ~A" *transmission-remote* (or args "")))))

(define-cli-tool :transmission-daemon (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *transmission-daemon* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (transmission-daemon-error "TRANSMISSION-DAEMON command failed: ~A ~A" *transmission-daemon* (or args "")))))
