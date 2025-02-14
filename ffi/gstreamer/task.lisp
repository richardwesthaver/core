;;; task.lisp --- GStreamer FFI Tasks

;; 

;;; Code:
(in-package :gstreamer)

(define-opaque gst-task)
(define-opaque gst-task-private)
(define-opaque gst-task-class)

(define-alien-enum (gst-task-state int)
                   :started 0
                   :stopped 1
                   :paused 2)

(define-alien-type gst-task-thread-func (* t))

(define-alien-type gst-task-t
  (struct gst-task
          (object gst-object-t)
          (state gst-task-state)
          (cond gcond)
          (lock (* grec-mutex))
          (func gst-task-thread-func)
          (user-data gpointer)
          (notify gdestroy-notify)
          (running boolean)
          (thread (* gthread))
          (priv (* gst-task-private))
          (%gst-reserved (array gpointer #.+gst-padding+))))
