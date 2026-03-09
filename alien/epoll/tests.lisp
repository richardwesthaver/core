;;; epoll/tests.lisp --- EPOLL Alien Tests
(defpackage :epoll/tests 
  (:use :cl :rt :std :epoll :sb-alien))

(in-package :epoll/tests)

(defsuite :epoll)
(in-suite :epoll)

(deftest sanity (:skip :todo))
