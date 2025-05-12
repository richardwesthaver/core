;;; ges.lisp --- GStreamer Editing Services

;; 

;;; Code:
(in-package :gstreamer)

(defar ges-init void (argc (* int)) (argv (* (* c-string))))
(defar ges-init-check boolean
  (argc (* int)) (argv (* (* c-string)))
  (error (* (* gerror))))
;; (ges-init-check)
(defar ges-is-initialized boolean)

(defar ges-deinit void)

(defar ges-version void (major (* unsigned-int)) (minor (* unsigned-int)) (micro (* unsigned-int)) (nano (* unsigned-int)))
