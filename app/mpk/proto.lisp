;;; proto.lisp --- MPK Protocols

;; 

;;; Code:
(in-package :mpk)

(define-logical-pathname "MPK" nil
  ;; ("**;*.*.*" (merge-pathnames "**/*.*" *mpk-user-directory*))
  ("MEDIA;**;*.*.*" (merge-pathnames "**/*.*" *mpk-media-directory*))
  ("USER;**;*.*.*" (merge-pathnames "**/*.*" *mpk-user-directory*))
  ("DATA;**;*.*.*" (merge-pathnames "**/*.*" *mpk-data-directory*))
  ("DB;**;*.*.*" (merge-pathnames "**/*.*" *mpk-db-directory*))
  ("CACHE;**;*.*.*" (merge-pathnames "**/*.*" *mpk-cache-directory*)))

(defclass mpk-object (id) ())

(defclass mpk-project (mpk-object skel:sk-project) ())

(defclass mpk-component (mpk-object skel:sk-component) ())

;; playback state
(defgeneric mpk-play (self &rest args &key &allow-other-keys)
  (:documentation "Play media object SELF.")
  (:method ((self pathname) &key &allow-other-keys)
    "Pathnames are ran through MIME-CASE and played with the associated default media player."
    (dat/mime:mime-case self
      ;; todo - aplay? snd? jack? gstreamer?
      ("audio/*" (run-mpv (namestring self)))
      ("video/*" (run-mpv (namestring self)))
      (t (nyi! (format nil "unknown file type: ~A" self)))))
  (:method ((self string) &key &allow-other-keys)
    "Strings are assumed to be song queries sent to MPD - matching tracks are played immediately."
    (mpd:ensure-mpc (*mpc*)
      (let ((id (parse-integer (mpd:mpc-add-id *mpc* self))))
        (mpd:mpc-play *mpc* id)
        id)))
  (:method ((self integer) &key &allow-other-keys)
    "An integer is assumed to refer to an index in the current MPD playlist."
    (mpd:ensure-mpc (*mpc*)
      (mpd:mpc-play *mpc* self)))
  (:method ((self null) &key &allow-other-keys)
    "A nil value resumes playback of the current playlist."
    (mpd:ensure-mpc (*mpc*)
      (mpd:mpc-play *mpc*))))

(defgeneric mpk-pause (self &rest args &key &allow-other-keys)
  (:documentation "Pause playback state of player object SELF.")
  (:method ((self (eql :mpd)) &key &allow-other-keys)
    (mpd:ensure-mpc (*mpc*)
      (mpd:mpc-pause *mpc*))))

(defgeneric mpk-toggle (self &rest args &key &allow-other-keys)
  (:documentation "Toggle playback state of player object SELF between :PLAYING and :PAUSED.")
  (:method ((self (eql :mpd)) &key &allow-other-keys)
    (mpd:ensure-mpc (*mpc*)
      (mpd:mpc-pause *mpc*))))

(defgeneric mpk-stop (self &rest args &key &allow-other-keys)
  (:documentation "Set playback state of object SELF to :STOPPED.")
  (:method ((self (eql :mpd)) &key &allow-other-keys)
    (mpd:ensure-mpc (*mpc*)
      (mpd:mpc-stop *mpc*))))

;; playlist state
(defgeneric mpk-shuffle (self &rest args &key &allow-other-keys)
  (:documentation "Toggle shuffling of object SELF.")
  (:method ((self (eql :mpd)) &key &allow-other-keys)
    (mpd:ensure-mpc (*mpc*)
      (mpd:mpc-shuffle *mpc*))))

(defgeneric mpk-next (self)
  (:documentation "Move to the next item in the playlist.")
  (:method ((self (eql :mpd)))
    (mpd:ensure-mpc (*mpc*)
      (mpd:mpc-next *mpc*))))
    
(defgeneric mpk-prev (self)
  (:documentation "Move to the previous item in the playlist.")
  (:method ((self (eql :mpd)))
    (mpd:ensure-mpc (*mpc*)
      (mpd:mpc-previous *mpc*))))
