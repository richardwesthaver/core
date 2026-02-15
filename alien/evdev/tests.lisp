;;; tests.lisp --- evdev FFI tests

;; 

;;; Code:
(defpackage :evdev/tests
  (:use :cl :std :log :rt :sb-alien :evdev))
(in-package :evdev/tests)
(defsuite :evdev)
(in-suite :evdev)
(load-evdev)
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
(deftest basic ()
  (labels ((input (&optional (n 0))
           (handler-case (open (format nil "/dev/input/event~A" n))
             (file-error () (input (incf n))))))
    (let* ((file (input))
           (dev (libevdev-new))
           (fd (sb-sys:fd-stream-fd file)))
      (is (typep dev '(alien (* evdev::libevdev))))
      (is (zerop (libevdev-set-fd dev fd)))
      (is (null (libevdev-free dev))))))
