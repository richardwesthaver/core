;;; lib/pod/api.lisp --- Podman API model

;;
#|
'podman info'

curl --unix-socket /run/podman/podman.sock http://d/v4.0.0/libpod/info

'podman pull quay.io/containers/podman'

curl -XPOST --unix-socket /run/podman/podman.sock -v 'http://d/v4.0.0/images/create?fromImage=quay.io%2Fcontainers%2Fpodman'

'podman list images'

curl --unix-socket /run/podman/podman.sock -v 'http://d/v4.0.0/libpod/images/json' | jq
|#
;;; Code:
(in-package :pod)

(defvar *libpod-params* (make-hash-table :test #'equal))

(defvar *libpod-paths* (make-hash-table :test #'equal))

(defun register-libpod-param (name prototype)
  (setf (gethash name *libpod-params*) prototype))

(defun register-libpod-path (name prototype)
  (setf (gethash name *libpod-params*) prototype))

(defmacro register-libpod-params (&rest forms)
  (dolist (f forms)
    (register-libpod-param (car f) (cdr f))))

(defmacro register-libpod-paths (&rest forms)
  (dolist (f forms)
    (register-libpod-path (car f) (cdr f))))

;; we should really group these better
(register-libpod-params ("caCertFile" string)
                        ("file" string)
                        ("kubeConfig" string)
                        ("namespace" string)
                        ("service" string)
                        ("detachKeys" string)
                        ("logs" boolean)
                        ("stderr" boolean)
                        ("stdin" boolean)
                        ("stdout" boolean)
                        ("stream" boolean)
                        ("name" string)
                        ("export" boolean)
                        ("fileLocks" boolean)
                        ("ignoreRootFS" boolean)
                        ("ignoreVolumes" boolean)
                        ("keep" boolean)
                        ("leaveRunning" boolean)
                        ("preCheckpoint" boolean)
                        ("printStats" boolean)
                        ("tcpEstablished" boolean)
                        ("withPrevious" boolean)
                        ("author" string)
                        ("changes" (vector string))
                        ("comment" string)
                        ("container" string)
                        ("format" string)
                        ("pause" boolean)
                        ("repo" string)
                        ("squash" boolean)
                        ("stream" boolean)
                        ("tag" string)
                        ("path" string)
                        ("depend" boolean)
                        ("force" boolean)
                        ("ignore" boolean)
                        ("timeout" int)
                        ("v" boolean)
                        ("until" time)
                        ("label" string)
                        ("label!" string)
                        ("names" (vector string))
                        ("noTrunc" boolean)
                        ("podmanOnly" boolean)
                        ("replicas" int)
                        ("service" boolean)
                        ("type" string)
                        ("additionalEnvVariables" (vector string)))

(register-libpod-paths
 ;; system
 ("libpod/info" (:get))
 ("libpod/_ping" (:get))
 ("libpod/system/df" (:get))
 ;; container
 ("libpod/containers/json" (:get (all filters limit namespace pod size sync)))
 ;; image
 ("libpod/images/json" (:get (all filters)))
 ;; pod
 ("libpod/pods/json" (:get (filters)))
 ;; volume
 ("libpod/volumes/json" (:get (filters)))
 ;; secret
 ("libpod/secrets/json" (:get (filters)))
 ;; network
 ("libpod networks/json" (:get (filters))))

(defstruct libpod-param 
  (name "" :type string)
  (val nil))

(defstruct (libpod-request (:conc-name "REQUEST-"))
  (path "" :type string)
  (method :get :type keyword)
  (params (make-array 0 :element-type 'libpod-param :fill-pointer 0 :adjustable t) :type (vector libpod-param))
  (body nil))

(defmethod push-param ((param libpod-param) (request libpod-request))
  (vector-push param (request-params request)))

(defmethod build-request ((self libpod-request))
  "Return a function that calls DEX:REQUEST with args filled in from
SELF. Holes are left open and exposed as arguments to the returned
function as in the below lambda-list:

(SOCKET ENDPOINT &OPTIONAL CALLBACK)."
  (compile nil 
           (lambda (socket endpoint &optional callback)
             (declare (ignorable socket endpoint callback)))))
  
(defstruct libpod-response 
  (status)
  (headers)
  (body nil :type (or null vector)))

;; (defmacro define-libpod-request (path))
;; (defmacro define-libpod-response (name))
