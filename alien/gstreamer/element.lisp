;;; element.lisp --- Gstreamer FFI Elements

;; 

;;; Code:
(in-package :gstreamer)

(eval-always
  (define-alien-enum (gst-state)
                     :void-pending 0
                     :null 1
                     :ready 2
                     :paused 3
                     :playing 4)
  (defun %state-change (state1 state2)
    (logior (ash (gst-state state1) 3) (gst-state state2)))
  (defun %elt-flag (n)
    (ash (gst-object-flags :last) n)))

(define-alien-enum (gst-state-change)
                   :null-to-ready (%state-change :null :ready)
                   :ready-to-paused (%state-change :ready :paused)
                   :paused-to-playing (%state-change :paused :playing)
                   :paused-to-ready (%state-change :paused :ready)
                   :ready-to-null (%state-change :ready :null)
                   :paused-to-paused (%state-change :paused :paused)
                   :playing-to-playing (%state-change :playing :playing))

(define-alien-enum (gst-state-change-return)
                   :failure 0
                   :success 1
                   :async 2
                   :no-preroll 3)

(define-alien-enum (gst-element-flags)
                   :locked-state (%elt-flag 0)
                   :sink (%elt-flag 1)
                   :source (%elt-flag 2)
                   :provide-clock (%elt-flag 3)
                   :require-clock (%elt-flag 4)
                   :indexable (%elt-flag 5)
                   :last (%elt-flag 10))

(define-alien-type gst-element
  (struct gst-element
          (object gst-object)
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

(defar gst-element-get-type gtype)

(macrolet ((gst-elt (name ret &rest args)
             `(defar ,(symbolicate "GST-ELEMENT-" name) ,ret (element (* gst-element)) ,@args)))
  (gst-elt provide-clock (* gst-clock))
  (gst-elt get-clock (* gst-clock))
  (gst-elt set-clock boolean (clock (* gst-clock)))
  (gst-elt set-base-time void (time gst-clock-time))
  (gst-elt get-base-time gst-clock-time)
  (gst-elt set-start-time void (time gst-clock-time))
  (gst-elt get-current-running-time gst-clock-time)
  (gst-elt get-current-clock-time gst-clock-time)
  (gst-elt set-bus void (bus (* gst-bus)))
  (gst-elt get-bus (* gst-bus))
  (gst-elt set-context void (context (* gst-context)))
  (gst-elt get-contexts (* glist))
  (gst-elt get-context (* gst-context) (context-type c-string))
  (gst-elt get-context-unlocked (* gst-context) (context-type c-string))
  (gst-elt add-pad boolean (pad (* gst-pad)))
  (gst-elt remove-pad boolean (pad (* gst-pad)))
  (gst-elt no-more-pads void)
  (gst-elt get-static-pad (* gst-pad) (name c-string))
  ;; deprecated: gst-element-get-request-pad
  ;; (gst-elt get-request-pad (* gst-pad) (name c-string))
  (gst-elt request-pad-simple (* gst-pad) (name c-string))
  ;; TODO
  ;; (gst-elt request-pad (* gst-pad) (templ (* gst-pad-template)) (name c-string) (caps (* gst-caps)))
  (gst-elt release-request-pad void (pad (* gst-pad)))
  (gst-elt iterate-pads (* gst-iterator))
  (gst-elt iterate-src-pads (* gst-iterator))
  (gst-elt iterate-sink-pads (* gst-iterator)))

(defar gst-element-get-state gst-state-change-return 
  (element (* gst-element)) 
  (state (* gst-state))
  (pending (* gst-state))
  (timeout gst-clock-time))

(defar gst-element-set-state gst-state-change-return (element (* gst-element)) (state gst-state))
