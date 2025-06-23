;;; openapi.lisp --- OpenAPI Service Generator

;; Parse an OpenAPI spec to generate REST clients and servers.

;;; Commentary:

;; ref: https://github.com/kat-co/openapi2cl/

;; ref: https://api.weather.gov/openapi.json

;; ref: https://developer.shodan.io/api/openapi.json

;;; Code:
(in-package :net/srv/openapi)


(defvar *default-openapi-version* "3.1.1")

(define-constant +openapi-pattern-extension-prefix+ "x-" :test 'string=)

;;; Objects
(macrolet ((defoapi (name fields args &rest body)
             (let ((sname (symbolicate "OPENAPI-" name)))
               `(progn 
                  (defstruct ,sname ,@fields)
                  (defun ,(symbolicate sname "-FROM-JSON") ,args
                    (declare (optimize (speed 3) (safety 0)))
                    ,@body)))))
  (defoapi info (title description terms contact license version) (obj)
    (make-openapi-info
     :title (json-getf obj "title")
     :description (json-getf obj "description")
     :version (json-getf obj "version")))

  (defoapi contact (name url email) (obj)
    (make-openapi-contact
     :name (json-getf obj "name")
     :url (when-let ((v (json-getf obj "url"))) (uri v))
     :email (json-getf obj "email")))

  (defoapi license (name url) (obj)
    (make-openapi-license
     :name (json-getf obj "name")
     :url (when-let ((v (json-getf obj "url"))) (uri v))))

  (defoapi server (url description variables) (obj)
    (make-openapi-server
     :url (when-let ((v (json-getf obj "url"))) (uri v))
     :description (json-getf obj "description")
     :variables (json-getf obj "variables")))

  (defoapi server-var (enum default description) (obj)
    (make-openapi-server-var
     :enum (json-getf obj "enum")
     :default (json-getf obj "default")
     :description (json-getf obj "description")))

  (defoapi components
      (schemas responses parameters examples request-bodies headers security-schemes links callbacks) (obj)
    (make-openapi-components
     :schemas (mapcar (lambda (x) (cons (car x) (deserialize (cadr x) :json-schema))) (ast (json-getf obj "schemas")))
     :responses (json-getf obj "responses")
     :parameters (json-getf obj "parameters")
     :examples (json-getf obj "examples")
     :headers (json-getf obj "headers")
     :links (mapcar 'openapi-link-from-json (json-getf obj "links"))
     :security-schemes (mapcar (lambda (x) (cons (car x) (openapi-security-scheme-from-json (cadr x))))
                               (ast (json-getf obj "securitySchemes")))))
     ;; TODO 2025-06-20: 

  ;; openapi-path
  (defoapi path-item ($ref summary description get put post delete options head patch trace servers parameters) (obj)
    (make-openapi-path-item
     :$ref (json-getf obj "$ref")))

  (defoapi operation 
      (tags summary description external-docs id parameters request-body responses callbacks deprecated security 
            servers) 
    (obj)
    (make-openapi-operation
     :tags (json-getf obj "tags")
     :summary (json-getf obj "summary")
     :description (json-getf obj "description")
     :external-docs (openapi-external-documentation-from-json obj)
     :id (json-getf obj "$id")
     :parameters (json-getf obj "parameters")
     :request-body (openapi-request-body-from-json obj)
     :responses (json-getf obj "responses")
     :callbacks (json-getf obj "callbacks")
     :deprecated (json-getf obj "deprecated")
     :security (json-getf obj "security")))

  (defoapi external-documentation (description url) (obj)
    (make-openapi-external-documentation 
     :description (json-getf obj "description") 
     :url (when-let ((v (json-getf obj "url"))) (uri v))))

  (defoapi parameter (name in description required deprecated allow-empty style explode allow-reserved schema examples content) (obj)
    (make-openapi-parameter
     :name (json-getf obj "name")
     :in (json-getf obj "in")
     :description (json-getf obj "description")
     :required (json-getf obj "required")
     :deprecated (json-getf obj "deprecated")
     :allow-empty (json-getf obj "allowEmpty")
     :style (json-getf obj "style")
     :explode (json-getf obj "explode")
     :allow-reserved (json-getf obj "allowReserved")
     :schema (deserialize obj :json-schema)
     :examples (json-getf obj "examples")
     :content (json-getf obj "content")))

  (defoapi request-body (description content required) (obj)
    (make-openapi-request-body
     :description (json-getf obj "description")
     :content (json-getf obj "content")
     :required (json-getf obj "required")))

  (defoapi media-type (schema examples encoding) (obj)
    (make-openapi-media-type
     :schema (deserialize obj :schema)
     :examples (json-getf obj "examples")
     :encoding (json-getf obj "encoding")))

  (defoapi encoding (content-type headers style explode allow-reserved) (obj)
    (make-openapi-encoding
     :content-type (json-getf obj "contentType")
     :headers (json-getf obj "headers")
     :style (json-getf obj "style")
     :explode (json-getf obj "explode")
     :allow-reserved (json-getf obj "allowReserved")))

  (defoapi response (default codes) (obj)
    (make-openapi-response
     :default (json-getf obj "default")
     :codes (remove "default" (ast obj) :test 'string= :key 'car)))

  (defoapi response-object (description headers content links) (obj)
    (make-openapi-response-object
     :description (json-getf obj "description")
     :headers (json-getf obj "headers")
     :content (json-getf obj "content")
     :links (mapcar 'openapi-link-from-json (json-getf obj "links"))))

  ;; callback

  (defoapi example (summary description value external-value) (obj)
    (make-openapi-example
     :summary (json-getf obj "summary")
     :description (json-getf obj "description")
     :value (json-getf obj "value")
     :external-value (json-getf obj "externalValue")))

  (defoapi link (operation-ref operation-id parameters request-body description server) (obj)
    (make-openapi-link
     :operation-ref (json-getf obj "operationRef")
     :operation-id (json-getf obj "operationId")
     :parameters (json-getf obj "parameters")
     :request-body (openapi-request-body-from-json obj)
     :description (json-getf obj "description")
     :server (openapi-server-from-json obj)))

  (defoapi header (description required deprecated allow-empty style explode allow-reserved schema examples content)
    (obj)
    (make-openapi-header
     :description (json-getf obj "description")
     :required (json-getf obj "required")
     :deprecated (json-getf obj "deprecated")
     :allow-empty (json-getf obj "allowEmpty")
     :style (json-getf obj "style")
     :explode (json-getf obj "explode")
     :allow-reserved (json-getf obj "allowReserved")
     :schema (deserialize (json-getf obj "scheme") :json-schema)
     :examples (json-getf obj "examples")
     :content (json-getf obj "content")))

  (defoapi tag (name description external-docs) (obj)
    (make-openapi-tag 
     :name (json-getf obj "name")
     :description (json-getf obj "description")
     :external-docs (json-getf obj "externalDocs")))

  ;; ref

  ;; openapi-schema

  (defoapi discriminator (name mapping) (obj)
    (make-openapi-discriminator :name (json-getf obj "name") :mapping (json-getf obj "mapping")))

  (defoapi xml (name namespace prefix attribute wrapped) (obj)
    (make-openapi-xml 
     :name (json-getf obj "name")
     :namespace (json-getf obj "namespace")
     :prefix (json-getf obj "prefix")
     :attribute (json-getf obj "attribute")
     :wrapped (json-getf obj "wrapped")))

  (defoapi security-scheme (type description name in scheme bearer-format flows open-id-connect-url user-agent) (obj)
    (make-openapi-security-scheme
     :type (json-getf obj "type")
     :description (json-getf obj "description")
     :name (json-getf obj "name")
     :in (json-getf obj "in")
     :scheme (json-getf obj "scheme")
     :bearer-format (json-getf obj "bearerFormat")
     :flows (json-getf obj "flows")
     :open-id-connect-url (when-let ((v (json-getf obj "openIdConnectUrl"))) (uri v))))
 
  (defoapi oauth-flow (implicit password client-credentials authorization-code) (obj)
    (make-openapi-oauth-flow
     :implicit (json-getf obj "implicit")
     :password (json-getf obj "password")
     :client-credentials (json-getf obj "clientCredentials")
     :authorization-code (json-getf obj "authorizationCode")))

  (defoapi oauth-flow-object (authorization-url token-url refresh-url scopes) (obj)
    (make-openapi-oauth-flow-object
     :authorization-url (when-let ((v (json-getf obj "authorizationUrl"))) (uri v))
     :token-url (when-let ((v (json-getf obj "tokenUrl"))) (uri v))
     :refresh-url (when-let ((v (json-getf obj "refreshUrl"))) (uri v))
     :scopes (json-getf obj "scopes"))))

(defun openapi-paths-from-json (obj)
  (mapcar (lambda (x) (cons (car x) (openapi-path-item-from-json (cadr x)))) (ast obj)))

(defun openapi-servers-from-json (obj)
  (mapcar 'openapi-server-from-json obj))

(defun openapi-tags-from-json (obj)
  (mapcar 'openapi-tag-from-json obj))

;; security-requirement


;;; Document
(defclass openapi-document (json-object) 
  ((spec-version :initarg :spec-version :initform *default-openapi-version*)
   (info :initarg :info :type openapi-info)
   (components :initarg :components :type openapi-components)
   (paths :initarg :paths)
   (servers :initarg :servers)
   (security :initarg :security)
   (tags :initarg :tags)
   (external-docs :initarg :external-docs)))

(defmethod deserialize ((self json-object) (format (eql :openapi)) &key)
  (flet ((%from (key fn)
           (declare (string key) (function fn))
           (when-let ((k (json-getf self key)))
             (funcall fn k))))
  (make-instance 'openapi-document
    :spec-version (json-getf self "openapi")
    :info (openapi-info-from-json (json-getf self "info"))
    :paths (openapi-paths-from-json (json-getf self "paths"))
    :servers (%from "servers" #'openapi-servers-from-json)
    :components (%from "components" #'openapi-components-from-json)
    :security (%from "security" (lambda (x) (mapcar 'ast x)))
    :tags (%from "tags" #'openapi-tags-from-json)
    :external-docs (%from "externalDocs" #'openapi-external-documentation-from-json))))

(defmethod deserialize ((self t) (format (eql :openapi)) &key)
  (deserialize (deserialize self :json) :openapi))

;;; Client
(defclass oapi-client (client) ())

;;; Server
(defclass oapi-server (http-server) ())

;;; Service
(defclass oapi-service (net-service oapi-server) ())
