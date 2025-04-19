;;; proto.lisp --- MPK Protocols

;; 

;;; Code:
(in-package :mpk)

(defclass mpk-object (id) ())

(defclass mpk-project (mpk-object skel:sk-project) ())
(defclass mpk-component (mpk-object skel:sk-component) ())

(defgeneric mpk-play (self &rest args &key &allow-other-keys)
  (:method ((self pathname) &key &allow-other-keys)
    (dat/mime:mime-case self
      ;; todo - aplay? snd? jack? gstreamer?
      ("audio/*" (run-mpv (namestring self)))
      ("video/*" (run-mpv (namestring self)))
      (t (nyi! (format nil "unknown file type: ~A" self)))))
  (:method ((self string) &key &allow-other-keys)
    (mpd:with-mpc (*mpc*)
      (let ((id (parse-integer (mpd:mpc-add-id *mpc* self))))
        (mpd:mpc-play *mpc* id)
        id))))

(defgeneric mpk-toggle (self &rest args &key &allow-other-keys))
(defgeneric mpk-pause (self &rest args &key &allow-other-keys)
  (:method ((self (eql :mpd)) &key &allow-other-keys)
    (mpd:with-mpc (*mpc*)
      (mpd:mpc-pause *mpc*))))
(defgeneric mpk-stop (self &rest args &key &allow-other-keys))
(defgeneric mpk-shuffle (self &rest args &key &allow-other-keys))
(defgeneric mpk-previous (self &rest args &key &allow-other-keys))
