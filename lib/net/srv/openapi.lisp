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
     :url (json-getf obj "url")
     :email (json-getf obj "email")))

  (defoapi license (name url) (obj)
    (make-openapi-license
     :name (json-getf obj "name")
     :url (json-getf obj "url")))

  (defoapi server (url description variables) (obj)
    (make-openapi-server
     :url (json-getf obj "url")
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
     :schemas (json-getf obj "schemas")
     :responses (json-getf obj "responses")
     :parameters (json-getf obj "parameters")
     :examples (json-getf obj "examples")
     :headers (json-getf obj "headers")
     :links (json-getf obj "links")))
     ;; TODO 2025-06-20: 

  ;; openapi-path
  (defoapi path-item ($ref summary description get put post delete options head patch trace servers parameters) (obj)
    (make-openapi-path-item
     :$ref (json-getf obj "$ref")))

  (defoapi operation 
      (tags summary 
            description external-docs 
            id parameters request-body 
            responses callbacks 
            deprecated security 
            servers) 
    (obj))

  (defoapi external-documentation (description url) (obj)
    (make-openapi-external-documentation :description (json-getf obj "description") :url (json-getf obj "url")))

  (defoapi parameter (name in description required deprecated allow-empty style explode allow-reserved schema examples content) (obj))

  (defoapi request-body (description content required) (obj))

  (defoapi media-type (schema examples encoding) (obj))

  (defoapi encoding (content-type headers style explode allow-reserved) (obj))

  (defoapi response (default codes) (obj))

  (defoapi response-object (description headers content links) (obj))

  ;; callback

  (defoapi example (summary description value external-value) (obj))

  (defoapi link (operation-ref operation-id parameters request-body description server) (obj))

  (defoapi header (description required deprecated allow-empty style explode allow-reserved schema examples content) (obj))

  (defoapi tag (name description external-docs) (obj)
    (make-openapi-tag 
     :name (json-getf obj "name")
     :description (json-getf obj "description")
     :external-docs (json-getf obj "externalDocs")))
     

  ;; ref

  ;; openapi-schema

  (defoapi discriminator (name mapping) (obj))

  (defoapi xml (name namespace prefix attribute wrapped) (obj))

  (defoapi security-scheme (type description name in scheme bearer-format flows open-id-connect-url) (obj))
 
  (defoapi oauth-flow (implicit password client-credentials authorization-code) (obj))
  (defoapi oauth-flow-object (authorization-url token-url refresh-url scopes) (obj)))

(defun openapi-paths-from-json (obj)
  (mapcar (lambda (x) (cons (car x) (openapi-path-item-from-json (cadr x)))) (ast:ast obj)))

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
    :security (%from "security" #'openapi-security-scheme-from-json)
    :tags (%from "tags" #'openapi-tags-from-json)
    :external-docs (%from "externalDocs" #'openapi-external-documentation-from-json))))

(defmethod deserialize ((self t) (format (eql :openapi)) &key)
  (deserialize (deserialize self :json) :openapi))
