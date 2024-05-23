;; lib/pod/util.lisp --- Pod utilities

;;

;;; Code:
(in-package :pod)

(defun decode-podman-response (buf))

(defun encode-podman-request (obj))

;;; HTTP/UDS
;; assumes socket-connect was already called
(defun format-libpod-api-local (path)
  (format nil "http://localhost/v~a/libpod/~a" *libpod-api-version* path))

(defun libpod-request (client path &optional (method :get))
  (let ((stream (socket-make-stream client
                                    :element-type 'octet
                                    :input t
                                    :output t
                                    :buffering :none)))
    ;; TODO 2024-04-01: remove dependencies
    (let ((wrapped-stream (flexi-streams:make-flexi-stream (chunga::make-chunked-stream stream)
                                                           :external-format :utf8)))
      (dex:request (format-libpod-api-local path) :method method :stream wrapped-stream))))

(defun libpod-request-json (client path &optional (method :get))
  (dat/json:json-decode (libpod-request client path method)))

;; (libpod-request-json *client* "_ping")
;; (libpod-request-json *client* "info")
;; (libpod-request-json *client* "events")
;; (libpod-request-json *client* "version")
;; (libpod-request-json *client* "containers/json")
