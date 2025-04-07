;;; transmission.lisp --- Transmission RPC

;; Transmission RPC

;;; Commentary:

;; ref: https://github.com/transmission/transmission/blob/main/docs/rpc-spec.md

#| Requests support three keys:

A required method string telling the name of the method to invoke

An optional arguments object of key/value pairs. The keys allowed are defined
by the method.

An optional tag number used by clients to track responses. If provided by a
request, the response MUST include the same tag.

{
   "arguments": {
     "fields": [
       "version"
     ]
   },
   "method": "session-get",
   "tag": 912313
}

|#

#| Responses to a request will include:

A required result string whose value MUST be success on success, or an error
string on failure.

An optional arguments object of key/value pairs. Its keys contents are defined
by the method and arguments of the original request.

An optional tag number.

{
   "arguments": {
      "version": "2.93 (3c5870d4f5)"
   },
   "result": "success",
   "tag": 912313
}

|#

;; Request/Response tags are represented via the ID superclass

;; Most transmission servers require a X-Transmission-Session-Id header to
;; prevent CSRF attacks. When you get a 409 error from the server, pull the
;; session id from the response and retry.

;; If you use authentication ALWAYS use HTTPS connection. Basic auth is used:
;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization#basic

;;; Code:
(in-package :net/proto/transmission)

;;; Vars
(defvar *transmission-url* (uri "http://localhost:9091/transmission/rpc"))
;;; Request
;; ref: https://github.com/j0rsa/transmission-rpc/blob/main/src/types/request.rs
(defclass transmission-request (request id) 
  ((method :initarg :method :type string :accessor request-method)
   (args :initform nil :initarg :args :accessor request-args)))

(defaccessor request-tag ((self transmission-request)) (id self))

(defvar *transmission-torrent-methods*
  '(:torrent-start :torrent-start-now :torrent-stop :torrent-verify 
    :torrent-reannounce :torrent-set :torrent-set-location :torrent-rename-path 
    :torrent-get :torrent-add :torrent-remove))

(defclass transmission-torrent-request (transmission-request) ())

(defvar *transmission-session-methods*
  '(:session-set :session-get :session-stats :blocklist-update :port-test :session-close
    :queue-move-top :queue-move-up :queue-move-down :queue-move-bottom
    :free-space :group-set :group-get))

(defclass transmission-session-request (transmission-request) ())

;;; Response
;; ref: https://github.com/j0rsa/transmission-rpc/blob/main/src/types/response.rs
(defclass transmission-response (response id)
  ((result :initarg :result :type string :accessor response-result)
   (args :initform nil :initarg :args :accessor response-args)))

(defaccessor response-tag ((self transmission-response)) (id self))

(declaim (inline response-ok-p response-error-p))
(defun response-ok-p (res)
  (string= "success" (response-result res)))
(defun response-error-p (res)
  (not (response-ok-p res)))

(defvar *transmission-http-request-prototype*
  (make-http-request :method :post))
