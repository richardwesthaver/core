;;; oauth.lisp --- OAuth2 Services

;; Services which communicate with external OAuth2 APIs and maintain local
;; authentication state.

;;; Code:
(in-package :net/srv/oauth)

(defstruct (oauth2-endpoint
            (:conc-name oauth2-))
  auth-url
  token-url
  tokeninfo-url
  revoke-url)

(defclass oauth2-client (id secret) ())

(defclass oauth2-service (service)
  ((client)
   (endpoint)
   (scopes)
   (token-type)
   (access-token)
   (refresh-token)
   (expiration)))

(defun get-auth-request-url (endpoint
                             &key
                             client
                             scopes                                 
                             (redirect-uri)
                             (state nil)
                             (extra-parameters nil))
  "Returns the URI to obtain the authentication code."
  (make-instance 'uri
    :path
    (slot-value endpoint 'auth-url)
    :query
    (delete nil
            (apply #'list
                   (cons "response_type" "code")
                   (cons "client_id" (id client))
                   (cons "redirect_uri" redirect-uri)
                   (when (not (null scopes))
                     (cons "scope" (format nil "~{~A~^ ~}" scopes)))
                   (when (not (null state))
                     (cons "state" (or state "")))
                   extra-parameters))))

(defun re-acquire-token (oauth)
  "Refreshes the token for the oauth2 object"
  (cond ((slot-value oauth 'refresh-token)
         (refresh-token oauth))
        (t
         (error "access token expired; no method of re-acquiring access"))))

(defun acquire-token-auth-code (oauth
                                auth-code
                                &key
                                redirect-uri)
  "Updates the oauth2 object with a token using the auth-code"
  (let ((now (get-universal-time))
        (json (%acquire-token-auth-code
               oauth
               :auth-code auth-code
               :redirect-uri redirect-uri)))
    (reset-from-json oauth now json))) ;; FIXME

(defun refresh-token (oauth)
  "Updates the oauth2 object with a refreshed token"
  (unless (slot-value oauth 'refresh-token)
    (error "refresh-token not available"))
  (let ((now (get-universal-time))
        (json (refresh-token-json oauth)))
    (reset-from-json oauth now json)))

(defun get-access-token (oauth
                         &key
                         (force t))
  "Returns the access-token for an oauth2 object"
  (cond ((or (null (slot-value oauth 'expiration))
             (< (get-universal-time) (slot-value oauth 'expiration)))
         (slot-value oauth 'access-token))
        (force
         (re-acquire-token oauth)
         (slot-value oauth 'access-token))
        (t nil)))

(defun reset-from-json (oauth now json-string)
  "Tool to update the oauth2 object"
  (let* ((json (json:json-decode json-string))
         (new-access-token (cdr (assoc :access--token json)))
         (new-token-type (cdr (assoc :token--type json)))
         (new-refresh-token (cdr (assoc :refresh--token json)))
         (new-scope (or (cdr (assoc :scope json))
                        ""))
         (new-expires-in (or (cdr (assoc :expires--in json))
                             'inf+)))
    (unless (string-equal new-token-type "Bearer")
      (error (format nil "unsupported token type ~a"
                     new-token-type)))
    (setf (slot-value oauth 'access-token) new-access-token)
    (setf (slot-value oauth 'token-type) new-token-type)
    (when new-refresh-token
      (setf (slot-value oauth 'refresh-token) new-refresh-token))
    (when (and (numberp now) (numberp new-expires-in))
      (setf (slot-value oauth 'expiration)
            (+ now new-expires-in)))
    (unless (equal new-scope "")
      (setf (slot-value oauth 'scopes) (split-sequence " +" new-scope)))
    nil
    ))

(defun %acquire-token-auth-code
    (oauth &key auth-code
                redirect-uri)
  "Using the given auth-code, pull the result from the oauth2-server"
  (req:post (slot-value (slot-value oauth 'endpoint) 'token-url)
            :headers (cons "Content-Type" "application/x-www-form-urlencoded")
            :content (%acquire-token-body oauth
                                          :auth-code auth-code
                                          :redirect-uri redirect-uri)))

(defun %refresh-token-body (oauth)
  "helper function to generate the refresh-token request"
  (list (cons "grant_type" "refresh_token")
        (cons "client_id" (id oauth))
        (cons "client_secret" (reveal (slot-value oauth 'client)))
        (cons "refresh_token" (slot-value oauth 'refresh-token))))

(defun %acquire-token-body (oauth
                            &key auth-code redirect-uri)
  "helper function to generate the acquire-token request"
  (list (cons "grant_type" "authorization_code")
        (cons "client_id" (id oauth))
        (cons "client_secret" (or (reveal (slot-value oauth 'client))
                                  ""))
        (cons "code" auth-code)
        (cons "redirect_uri" redirect-uri)))

(defun refresh-token-json (oauth)
  "Calls the server for an updated refresh-token info"
  (req:post
   (slot-value (slot-value oauth 'endpoint) 'token-url)
   :headers (cons "Content-Type" "application/x-www-form-urlencoded")
   :content (%refresh-token-body oauth)))

(defun headers (oauth)
  "Returns the authorization headers"
  (list (cons "Authorization"
              (format nil "Bearer ~a" (slot-value oauth 'access-token)))))

(defun oauth2-auth-code (endpoint client auth-code &key redirect-uri)
  "Returns an oauth2 object using the given auth-code to request an access
token. The redirect-uri must match the auth-code redirect-uri."
  (let ((oauth (make-instance 'oauth2-service
                 :endpoint endpoint
                 :client client)))
    (acquire-token-auth-code
     oauth
     auth-code
     :redirect-uri redirect-uri)
    oauth))

(defun oauth2-refresh-token
    (endpoint client refresh-token)
  "Returns an oauth2 object using the given refresh-token to acquire a new
access token."
  (let ((oauth (make-instance 'oauth2
                 :endpoint endpoint
                 :client client)))
    (setf (slot-value oauth 'refresh-token) refresh-token)
    (refresh-token oauth)
    oauth))

(defun servlet (&key (port 5000))
  "Start a blocking servlet that waits for code and state."
  (let ((srv (make-instance 'net/srv/http:http-service
               :port port))
        (code-res nil)
        (state-res nil))
    (unwind-protect
         (progn 
           (start srv)
           (defroute
               (oauth :uri "/oauth") (code state)
             (setf (content-type*) "text/plain")
             (setf code-res code)
             (setf state-res state)
             (format nil "Success"))
           (loop until code-res
                 return (list (cons "code" code-res)
                              (cons "state" state-res)))))
    (stop srv)))

(defun request-auth-code-with-browser (endpoint client
                                       &key
                                       (scopes nil)
                                       (port 5000))
  "Given an endpoint, an auth-client object, and a list of strings
defining the scope, initiates the authentication process."
  (let* ((redirect-uri (format nil "http://127.0.0.1:~a/oauth" port))
         (auth-url (get-auth-request-url endpoint
                                         :client client
                                         :scopes scopes
                                         :redirect-uri
                                         redirect-uri)))
    (browse-url auth-url)
    (let ((auth-code (cdr (assoc "code" (servlet :port port) :test #'string-equal))))
      (oauth2-auth-code endpoint client auth-code
                        :redirect-uri redirect-uri))))
