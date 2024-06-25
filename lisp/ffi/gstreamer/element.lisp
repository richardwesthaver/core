;;; element.lisp --- Gstreamer FFI Elements

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-enum (gst-state int)
                   :void-pending 0
                   :null 1
                   :ready 2
                   :paused 3
                   :playing 4)

(define-alien-enum (gst-state-change-return int)
                   :failure 0
                   :success 1
                   :async 2
                   :no-preroll 3)

(define-opaque gst-element)

(define-alien-type gst-element-t
  (struct gst-element
          (object gst-object-t)
          (state-lock grec-mutex)
          (state-cookie (unsigned 32))
          (target-state gst-state)
          (current_state gst-state)
          (next-state gst-state)
          (pending-state gst-state)
          (last-return gst-state-change-return)
          (bus (* gst-bus))
          (clock (* gst-clock))
          (base-time gst-clock-time-diff)
          (start-time gst-clock-time)
          (numpads (unsigned 16))
          (pads (* glist))
          (numsrcpads (unsigned 16))
          (srcpads (* glist))
          (numsinkpads (unsigned 16))
          (sinkpads (* glist))
          (pads-cookie (unsigned 32))
          (contexts (* glist))
          (%gst-reserved (array gpointer #.(- +gst-padding+ 1)))))


(define-opaque gst-element-class)
