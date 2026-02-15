;;; tests.lisp --- Gstreamer FFI Tests

;; 

;;; Code:
(defpackage :gstreamer/tests
  (:use :cl :std :rt :gstreamer :sb-alien))

(in-package :gstreamer/tests)
(defsuite :gstreamer)
(in-suite :gstreamer)

(glib:load-glib)
(load-gstreamer)
(load-gst-play)

(deftest sanity ()
  (with-alien ((argv (* c-string))
               (argc int 0)
               (major unsigned)
               (minor unsigned)
               (micro unsigned)
               (nano unsigned))
    (gst-init (addr argc) (addr argv))
    (gst-version (addr major) (addr minor) (addr micro) (addr nano))
    (log:info! "initialized GStreamer: ~A.~A.~A.~A~%" major minor micro nano)
    (let ((elt (gst-element-factory-make "fakesrc" "source")))
      (isnt (null-alien elt)))))

(deftest element-factory ()
  (with-alien ((f (* gst-element-factory) (gst-element-factory-find "fakesrc")))
    (let ((elt (gst-element-factory-create f "source")))
      (gst-object-unref elt)
      (gst-object-unref f))))

(deftest gobject-elements ()
  (with-alien ((elt (* gst-element) (gst-element-factory-make "fakesrc" "source"))
               (vals (array (* t))))
    (glib:g-object-getv (cast elt (* glib:gobject)) 1 (clone-strings (list "name")) vals)
    (gst-object-unref elt)))

(deftest basic ()
  (with-alien ((argv (* c-string))
	       (argc int 0))
    (gst-init (addr argc) (addr argv))
    (with-alien ((pipeline (* gst-element)
                           (gst-parse-launch 
                            "playbin uri=https://gstreamer.freedesktop.org/data/media/sintel_trailer-480p.webm"
                            nil)))
      ;; if we sleep here the video will play in a new X window
      (iseq :async (gst-state-change-return* (gst-element-set-state pipeline (gst-state :playing))))
      (with-alien ((bus (* gst-bus) (gst-element-get-bus pipeline)))
        (with-alien ((msg (* gst-message) (gst-bus-timed-pop-filtered bus (gst-clock-time :none))))
	  (when (= (slot msg 'gstreamer::type) (gst-message-type :error))
            (error "GStreamer error occurred"))
          (gst-message-unref msg)
          (gst-object-unref bus)
          (gst-element-set-state pipeline (gst-state :null))
          (gst-object-unref pipeline))))))
