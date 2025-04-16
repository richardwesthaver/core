;;; ges.lisp --- GStreamer Editing Services

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-routine ges-init void (argc (* int)) (argv (* (* c-string))))
(define-alien-routine ges-init-check boolean
  (argc (* int)) (argv (* (* c-string)))
  (error (* (* gerror))))
;; (ges-init-check)
(define-alien-routine ges-is-initialized boolean)

(define-alien-routine ges-deinit void)

(define-alien-routine ges-version void (major (* unsigned-int)) (minor (* unsigned-int)) (micro (* unsigned-int)) (nano (* unsigned-int)))
