;;; tests.lisp --- MPV FFI Tests

;; 

;;; Code:
(defpackage :mpv/tests
  (:use :cl :std :log :rt :mpv :sb-alien))

(in-package :mpv/tests)
(defsuite :mpv)
(in-suite :mpv)

(load-mpv)
(defvar *test-mpv* nil)
(deftest sanity ()
  (istype 'integer (mpv-client-api-version))
  ;; why does this work?
  (sb-int:with-float-traps-masked (:invalid) 
    (istype '(alien (* mpv-handle)) (setq *test-mpv* (mpv-create)))
    (iszero (mpv-set-option-string *test-mpv* "input-default-bindings" "yes"))
    (with-alien ((i boolean t))
      (iszero (mpv::mpv-set-option *test-mpv* "osc" (mpv-format :flag) (addr i))))
    (iszero (mpv-initialize *test-mpv*))
    (iszero (mpv-command-string *test-mpv*
                                "loadfile /mnt/z/music/7323889_Weltschmerz_Original_Mix.wav"))
  (unless (null-alien *test-mpv*)
    (setq *test-mpv* (mpv-terminate-destroy *test-mpv*)))))

