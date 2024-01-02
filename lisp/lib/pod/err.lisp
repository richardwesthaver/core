;;; lib/pod/err.lisp --- Pod Errors

;;

;;; Code:
(in-package :pod)

(define-condition pod-error (error) ())

(define-condition podman-error (pod-error) ())

;; Errors returned from LIBPOD API
(define-condition libpod-error (pod-error) 
  ((status :initform 200 :type integer)))

(defmacro def-libpod-err (err code)
  `(define-condition ,(symbolicate 'libpod- err '-error) (libpod-error) 
     ((status :initform ,code :type integer))))

(def-libpod-err no-such-container 404)
(def-libpod-err container-is-paused 409)
(def-libpod-err internal 500)
(def-libpod-err no-such-exec-instance 404)
(def-libpod-err container-is-not-running 409)
(def-libpod-err bad-parameter 400)
(def-libpod-err no-such-image 404)
(def-libpod-err conflict-in-operation 409)
(def-libpod-err no-such-manifest 404)
(def-libpod-err partial-success 409)
(def-libpod-err no-such-network 404)
(def-libpod-err status-conflict 409)
(def-libpod-err no-such-pod 404)
(def-libpod-err kill-pod 409)
(def-libpod-err pause-pod 409)
(def-libpod-err pod-already-exists 409)
(def-libpod-err restart-pod 409)
(def-libpod-err start-pod 409)
(def-libpod-err stop-pod 409)
(def-libpod-err unpause-pod 409)
(def-libpod-err pod-already-started 304)
(def-libpod-err no-such-volume 404)
(def-libpod-err volume-in-use 409)
(def-libpod-err no-such-secret 404)
