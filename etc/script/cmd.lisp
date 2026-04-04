#!/bin/core --script
#|A dummy command script for interactive testing.
|#
(use-package :cmd)
(init :log :level :trace)
(init :commands :name :cli :class 'cli-command)
(defcommand (:cli :cmd-optional) (&optional (arg "bar"))
  (declare (interactive (ustring "Insert a string (default=foo): ")))
  (println arg))

(let ((*interactive-optional-args-p* nil))
  (call-interactively "cmd-optional" '())) ;; "bar"
(let ((*interactive-optional-args-p* t))
  (call-interactively "cmd-optional" '("test")) ;; "TEST"
  (call-interactively "cmd-optional" (args))) ;; user input, default to "bar"
