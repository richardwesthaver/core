;; lib/pod/util.lisp --- Pod utilities

;;

;;; Code:
(in-package :pod)

(defun decode-podman-response (buf))

(defun encode-podman-request (obj))

;; podman system service --time=0 unix:///tmp/podman.sock (local-socket)
;; podman system service --time=0 tcp://localhost:8888 (inet-socket :stream :tcp)
(defun podman-run-command ())

(defun start-podman-service (addr &optional (time 0))
  "Start the Libpod API on ADDR which should be a valid uri beginning
with tcp:// or unix://."
  (sb-ext:run-program *podman-exe* `("system" "service" ,addr ,(format nil "--time=~a" time))))

;;; HTTP/UDS
;; assumes socket-connect was already called
(defun format-libpod-api-local (path)
  (format nil "http://localhost/v~a/libpod/~a" *libpod-api-version* path))

(defun libpod-request (client path &optional (method :get))
  (let ((stream (socket-make-stream client
                                    :element-type '(unsigned-byte 8)
                                    :input t
                                    :output t
                                    :buffering :none)))
    (let ((wrapped-stream (flexi-streams:make-flexi-stream (dex.usocket::make-chunked-stream stream)
                                                           :external-format :utf8)))
      (dex:request (format-libpod-api-local path) :method method :stream wrapped-stream))))

(defun libpod-request-json (client path &optional (method :get))
  (dat/json:json-decode (libpod-request client path method)))

;; (libpod-request-json *client* "_ping")
;; (libpod-request-json *client* "info")
;; (libpod-request-json *client* "events")
;; (libpod-request-json *client* "version")
;; (libpod-request-json *client* "containers/json")
