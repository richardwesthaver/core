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

(defstruct podman-request path method params body)

(defstruct podman-response status body)

(defstruct podman-query-param name val)

;; (defmacro define-podman-req/res (name))

(defgeneric attach-query-param (req))
(defgeneric send-request (req client callback))
(defgeneric receive-response (req client callback))
