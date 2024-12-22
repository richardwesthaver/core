;;; pkg.lisp --- MPV Packages

;; 

;;; Code:
(defpackage :mpv
  (:use :cl :std :sb-alien)
  (:export
   :load-mpv
   :mpv-handle
   :mpv-create
   :mpv-initialize
   :mpv-client-api-version
   :mpv-error
   :mpv-error-string
   :mpv-free
   :mpv-client-name
   :mpv-client-id
   :mpv-destroy
   :mpv-terminate-destroy
   :mpv-create-client
   :mpv-create-weak-client
   :mpv-load-config-file
   :mpv-get-time-ns
   :mpv-get-time-us
   :mpv-format
   :mpv-set-option
   :mpv-set-option-string
   :mpv-command
   :mpv-command-string
   :mpv-command-async
   :mpv-set-property-string
   :mpv-set-property
   :mpv-del-property
   :mpv-set-property-async
   :mpv-get-property
   :mpv-get-property-string
   :mpv-get-property-osd-string
   :mpv-get-property-async
   :mpv-observe-property
   :mpv-unobserve-property
   :mpv-event-id
   :mpv-event-name
   :mpv-event-property
   :mpv-log-level
   :mpv-event-log-message
   :mpv-end-file-reason
   :mpv-event-start-file
   :mpv-event-end-file
   :mpv-event-client-message
   :mpv-event-hook
   :mpv-event-command
   :mpv-event
   :mpv-event-to-node
   :mpv-request-event
   :mpv-request-log-messages
   :mpv-wait-event
   :mpv-wakeup
   :mpv-set-wakeup-callback
   :mpv-wait-async-requests
   :mpv-hook-add
   :mpv-hook-continue))

(in-package :mpv)

(define-alien-loader :mpv "/usr/lib/")

(define-alien-type mpv-handle (struct mpv-handle))

(define-alien-routine mpv-create (* mpv-handle))
(define-alien-routine mpv-initialize int (ctx (* mpv-handle)))

(define-alien-routine mpv-client-api-version unsigned-long)

(define-alien-enum (mpv-error int)
  :success 0
  :event-queue-full -1
  :nomem -2
  :uninitialized -3
  :invalid-parameter -4
  :option-not-found -5
  :option-format -6
  :option-error -7
  :property-not-found -8
  :property-format -9
  :property-unavailable -10
  :property-error -11
  :command -12
  :loading-failed -13
  :ao-init-failed -14
  :vo-init-failed -15
  :nothing-to-play -16
  :unknown-format -17
  :unsupported -18
  :not-implemented -19
  :generic -20)

(define-alien-routine mpv-error-string c-string (err int))

(define-alien-routine mpv-free void (data (* t)))

(define-alien-routine mpv-client-name c-string (ctx (* mpv-handle)))
(define-alien-routine mpv-client-id long (ctx (* mpv-handle)))

(define-alien-routine mpv-destroy void (ctx (* mpv-handle)))
(define-alien-routine mpv-terminate-destroy void (ctx (* mpv-handle)))
(define-alien-routine mpv-create-client (* mpv-handle) (ctx (* mpv-handle)) (name c-string))
(define-alien-routine mpv-create-weak-client (* mpv-handle) (ctx (* mpv-handle)) (name c-string))
(define-alien-routine mpv-load-config-file int (ctx (* mpv-handle)) (filename c-string))
(define-alien-routine mpv-get-time-ns long (ctx (* mpv-handle)))
(define-alien-routine mpv-get-time-us long (ctx (* mpv-handle)))

(define-alien-enum (mpv-format int)
  :none 0
  :string 1
  :osd-string 2
  :flag 3
  :int64 4
  :double 5
  :node 6
  :node-array 7
  :node-map 8
  :byte-array 9)

;; (define-alien-type mpv-node (struct mpv-node))
;; (define-alien-type mpv-node-list (struct mpv-node-list))
(define-alien-type mpv-byte-array (struct mpv-byte-array (data (* t)) (size size-t)))


;; (define-alien-routine mpv-free-node-contents void (node (* mpv-node)))

(define-alien-routine mpv-set-option int 
  (ctx (* mpv-handle)) (name c-string) (format mpv-format) (data (* t)))

(define-alien-routine mpv-set-option-string int 
  (ctx (* mpv-handle)) (name c-string) (data (* t)))
  
(define-alien-routine mpv-command int (ctx (* mpv-handle)) (args (array c-string)))

;; (define-alien-routine mpv-command-node int 
;;   (ctx (* mpv-handle)) 
;;   (args (* mpv-node))
;;   (result (* mpv-node)))

;; (define-alien-routine mpv-command-ret int (ctx (* mpv-handle)) (args (array c-string)) (result (* mpv-node)))
(define-alien-routine mpv-command-string int (ctx (* mpv-handle)) (args c-string))
(define-alien-routine mpv-command-async int (ctx (* mpv-handle)) (reply-userdata unsigned-long)
  (args (array c-string)))

;; (define-alien-routine mpv-command-node-async int (ctx (* mpv-handle)) (reply-userdata unsigned-long)
;;   (args (* mpv-node)))

(define-alien-routine mpv-abort-async-command void (ctx (* mpv-handle)) (repl-userdata unsigned-long))
(define-alien-routine mpv-set-property int (ctx (* mpv-handle)) (name c-string) (format mpv-format)
  (data (* t)))
(define-alien-routine mpv-set-property-string int (ctx (* mpv-handle)) (name c-string) (data c-string))

(define-alien-routine mpv-del-property int (ctx (* mpv-handle)) (name c-string))

(define-alien-routine mpv-set-property-async int (ctx (* mpv-handle)) (reply-userdata unsigned-long)
  (name c-string) (format mpv-format) (data (* t)))

(define-alien-routine mpv-get-property int (ctx (* mpv-handle)) (name c-string) (format mpv-format)
  (data (* t)))

(define-alien-routine mpv-get-property-string c-string (ctx (* mpv-handle)) (name c-string))
(define-alien-routine mpv-get-property-osd-string c-string (ctx (* mpv-handle)) (name c-string))

(define-alien-routine mpv-get-property-async int (ctx (* mpv-handle)) (repl-userdata unsigned-long)
  (name c-string) (format mpv-format))

(define-alien-routine mpv-observe-property int (mpv (* mpv-handle)) (reply-userdata unsigned-long)
  (name c-string) (format mpv-format))

(define-alien-routine mpv-unobserve-property int (mpv (* mpv-handle)) (registered-reply-userdata unsigned-long))

(define-alien-enum (mpv-event-id int)
  :none 0
  :shutdown 1
  :log-message 2
  :get-property-reply 3
  :set-property-reply 4
  :command-reply 5
  :start-file 6
  :end-file 7
  :file-loaded 8
  ;;deprecated idle and tick
  :client-message 16
  :video-reconfig 17
  :audio-reconfig 18
  :seek 20
  :playback-restart 21
  :property-change 22
  :queue-overflow 24
  :event-hook 25)

(define-alien-routine mpv-event-name c-string (event mpv-event-id))

(define-alien-type mpv-event-property
  (struct mpv-event-property
    (name c-string)
    (format mpv-format)
    (data (* t))))

(define-alien-enum (mpv-log-level int)
  :none 0
  :fatal 10
  :error 20
  :warn 30
  :info 40
  :v 50
  :debug 60
  :trace 70)

(define-alien-type mpv-event-log-message
  (struct mpv-event-log-message
    (prefix c-string)
    (level c-string)
    (text c-string)
    (log-level mpv-log-level)))

(define-alien-enum (mpv-end-file-reason int)
  :eof 0
  :stop 2
  :quit 3
  :error 4
  :redirect 5)

(define-alien-type mpv-event-start-file
  (struct mpv-event-start-file
    (playlist-entry-id long)))

(define-alien-type mpv-event-end-file
  (struct mpv-event-end-file
    (reason mpv-end-file-reason)
    (error int)
    (playlist-entry-id long)
    (playlist-insert-id long)
    (playlist-insert-num-entries int)))
    
(define-alien-type mpv-event-client-message
  (struct mpv-event-client-message
    (num-args int)
    (args (array c-string))))

(define-alien-type mpv-event-hook
  (struct mpv-event-hook
    (name c-string)
    (id unsigned-long)))

;; (define-alien-type mpv-event-command
;;   (struct mpv-event-command
;;     (result mpv-node)))

(define-alien-type mpv-event
  (struct mpv-event
    (event-id mpv-event-id)
    (error int)
    (reply-userdata unsigned-long)
    (data (* t))))

;; (define-alien-type mpv-event-to-node int
;;   (dst (* mpv-node))
;;   (src (* mpv-event)))

(define-alien-routine mpv-request-event int
  (ctx (* mpv-handle))
  (event mpv-event-id)
  (enable int))

(define-alien-routine mpv-request-log-messages int
  (ctx (* mpv-handle))
  (min-level c-string))

(define-alien-routine mpv-wait-event (* mpv-event)
  (ctx (* mpv-handle))
  (timeout double))

(define-alien-routine mpv-wakeup void
  (ctx (* mpv-handle)))

(define-alien-routine mpv-set-wakeup-callback void
  (ctx (* mpv-handle))
  ;; TODO
  (cb (function (* t) (* t)))
  (d (* t)))

(define-alien-routine mpv-wait-async-requests void
  (ctx (* mpv-handle)))

(define-alien-routine mpv-hook-add int
  (ctx (* mpv-handle))
  (reply-userdata unsigned-long)
  (name c-string)
  (priority int))

(define-alien-routine mpv-hook-continue int
  (ctx (* mpv-handle))
  (id unsigned-long))
