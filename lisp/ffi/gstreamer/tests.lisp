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
  (with-alien ((argv (array c-string))
               (argc int 0)
               (major unsigned)
               (minor unsigned)
               (micro unsigned)
               (nano unsigned))
    (gst-init (addr argc) argv)
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
               (vals (* (* t))))
    (g-object-getv (cast elt (* glib:gobject)) 1 (clone-strings (list "name")) vals)
    (print (deref vals 0))
    (gst-object-unref elt)))

      
