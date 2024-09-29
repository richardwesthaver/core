;;; tests.lisp --- evdev FFI tests

;; 

;;; Code:
(defpackage :evdev/tests
  (:use :cl :std :log :rt :sb-alien :evdev))
(in-package :evdev/tests)
(defsuite :evdev)
(in-suite :evdev)
(load-evdev nil)
(deftest sanity ())
  
#|
struct libevdev *dev;
int err;

dev = libevdev_new();
if (!dev)
        return ENOMEM;

err = libevdev_set_fd(dev, fd);
if (err < 0)
        printf("Failed (errno %d): %s\n", -err, strerror(-err));

libevdev_free(dev);
|#
(deftest basic ())
