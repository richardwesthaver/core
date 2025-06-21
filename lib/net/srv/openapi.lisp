;;; openapi.lisp --- OpenAPI Service Generator

;; Parse an OpenAPI spec to generate REST clients and servers.

;;; Commentary:

;; ref: https://github.com/kat-co/openapi2cl/

;; ref: https://api.weather.gov/openapi.json

;; ref: 

;;; Code:
(in-package :net/srv/openapi)


(defvar *default-openapi-version* "3.1.1")

(define-constant +openapi-pattern-extension-prefix+ "x-" :test 'string=)

(defstruct openapi-info title description terms contact license version)

(defstruct openapi-contact name url email)

(defstruct openapi-license name url)  

(defstruct openapi-server url description variables)

(defstruct openapi-server-var enum default description)

(defstruct openapi-component 
  schemas responses parameters examples request-bodies headers security-schemes links callbacks)

;; openapi-path
(defstruct openapi-path-item 
  $ref summary description get put post delete options head patch trace servers parameters)

(defstruct openapi-operation 
  tags summary description external-docs id parameters request-body responses callbacks deprecated security servers)

(defstruct openapi-external-documentation description url)

(defstruct openapi-parameter name in description required deprecated allow-empty style explode allow-reserved schema examples content)

(defstruct openapi-request-body description content required)

(defstruct openapi-media-type schema examples encoding)

(defstruct openapi-encoding content-type headers style explode allow-reserved)

(defstruct openapi-response default codes)

(defstruct openapi-response-object description headers content links)

;; callback

(defstruct openapi-example summary description value external-value)

(defstruct openapi-link operation-ref operation-id parameters request-body description server)

(defstruct openapi-header description required deprecated allow-empty style explode allow-reserved schema examples content)

(defstruct openapi-tag name description external-docs)

;; ref

;; openapi-schema

(defstruct openapi-discriminator name mapping)

(defstruct openapi-xml name namespace prefix attribute wrapped)

(defstruct openapi-security-scheme type description name in scheme bearer-format flows open-id-connect-url)

(defstruct openapi-oauth-flow implicit password client-credentials authorization-code)
(defstruct openapi-oauth-flow-object authorization-url token-url refresh-url scopes)

;; security-requirement

(defclass openapi-document (json-object) 
  ((spec-version :initarg :spec-version :initform *default-openapi-version*)
   (info :initarg :info :type openapi-info)
   (components)
   (paths)
   (servers)
   (definitions)
   (parameters)
   (responses)
   (security)
   (tags)
   (external-docs)))
