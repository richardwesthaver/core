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
(deftest sanity ()
  (with-alien ((argv (array c-string))
               (argc (* t))
               (major unsigned)
               (minor unsigned)
               (micro unsigned)
               (nano unsigned))
    (gst-init nil argv)
    (gst-version (addr major) (addr minor) (addr micro) (addr nano))
    (format t "initialized GStreamer: ~A.~A.~A.~A~%" major minor micro nano)
    (gst-deinit)
    (println "Shutdown GStreamer")))

(deftest basic ())
