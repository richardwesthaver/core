#!/usr/bin/env -S core --script
#|A dummy command script for interactive testing.
|#
(use-package :cmd)
(init :log :level :trace)
(init :commands :name :cli :class 'cli-command)

(defcommand (:cli :cmd-optional) (&optional (arg "bar"))
  (declare (interactive (ustring "Insert a string (default=foo): ")))
  (println arg))

(defcommand (:cli :cmd-rest) (&rest args)
  (declare (interactive (* "&REST: ")))
  (println args))

(let ((*interactive-rest-args-p*))
  (defcommand (:cli :cmd-keys) (&key (foo "foo") (bar "bar") baz)
    (declare (interactive string string string))
    (println (list foo bar baz))))

;; (let ((*interactive-optional-args-p* nil))
;;   (call-interactively "cmd-optional" '())) ;; "bar"

;; (let ((*interactive-optional-args-p* t))
;;   (call-interactively "cmd-optional" '("test")) ;; "TEST"
;;   (call-interactively "cmd-optional" (args))) ;; user input, default to "bar"

(let ((*interactive-key-args-p* nil))
  (call-interactively "cmd-keys" '(:baz "test"))
  (call-interactively "cmd-keys" (cli-args))) ;; default to ("foo" "bar" NIL)
