;;; jack.lisp --- JACK API

;; 

;;; Code:
(in-package :dsp/aud)

(defparameter *jack-snd* nil)

(defvar *dac-folding* t) ;; fold in-chans around out-chans

(defun jack-open-sound (path)
  (if (probe-file path)
      (let (snd ch)
        (when *jack-snd* (sf-close *jack-snd*))
        (with-sf-info (i)
          (setf snd (sf-open (namestring path) (sf-flag :read) (sb-alien:addr i))
                ch (sb-alien:slot i 'channels))
          (values snd ch)))
      (error 'sb-ext:file-does-not-exist :pathname path)))

(defparameter *jack-reading* t)
(defparameter *output-channels* 2)
(defconstant +sample-size+ (sb-alien:alien-size jack::jack-default-audio-sample-t))
(defparameter *frame-bytes* (* +sample-size+ *output-channels*))

(defun print-jack-snd (obj stream depth)
  (declare (ignore depth))
  (print-unreadable-object (obj stream)
    (format stream "jack-sound: ~S playing: ~A proc: ~A"
	    (file-namestring (jack-snd-path obj))
	    (jack-snd-playing? obj)
	    (jack-snd-disk-proc obj))))

(defstruct (jack-snd (:print-function print-jack-snd))
  path
  sound-file-handle
  chans
  ringbuffer
  disk-proc
  poker
  playing?
  start					;nil or position (millisec.)
  loop?					;nil or (start . end)
  outbus				;bus to feed ch-0, succeeding channels thereafter
  )

;;; global list of jack-snd objects, looked up in disk-threads and Jacks
;;; server-callback process

(defparameter *jack-sounds* nil)

;;; control: open, play, stop, pause, unpause, seek, loop, close, clenaup...

(defun jack-seek (sf frame)
  (sf-seek sf frame 0))

(defun jack-play-sound (soundfile &optional start loop? (tracknum 0))
  (multiple-value-bind (mysf chans) (jack-open-sound soundfile)
    (when mysf
      (let ((jack-snd (make-jack-snd :path soundfile
				     :sound-file-handle mysf
				   :chans chans
				   :ringbuffer (jack-ringbuffer-create (* +sample-size+ *output-channels* (ash 1 15)))
				   :playing? t
				   :start (if start (ms->frame start))
				   :loop? loop?
				   :outbus tracknum)))
	(when (numberp start) (jack-seek mysf (ms->frame start)))
	(let ((thisproc (make-thread 'disk-to-ringbuffer-proc
                                     :name (format nil "jack-producer ~S" (file-namestring soundfile))
				     :arguments (list jack-snd))))
	  (setf (jack-snd-disk-proc jack-snd) thisproc)
	  (pushnew jack-snd *jack-sounds*)
	  jack-snd)))))

(defun jack-close-sound (sound)
  (let ((snd (find sound *jack-sounds*)))
    (cond (snd
	   (setf *jack-sounds* (remove snd *jack-sounds*))
	   (setf (jack-snd-playing? snd) nil)
	   (foreign-free (jack-snd-ringbuffer snd))
	   (sf-close (jack-snd-sound-file-handle snd))
	   (kill-thread (jack-snd-disk-proc snd)))
	  (t nil)))) ;;(warn "didnt find sound: ~A in *jack-sounds*" snd)
	     
(defun jack-close-all-sounds (&optional (sounds *jack-sounds*))
  (dolist (s sounds)
    (jack-close-sound s)))

(defun n-sounds-playing-now (sounds)
  (count-if #'jack-snd-playing? sounds))

(defun n-sounds-pausing-now (sounds)
  (count-if-not #'jack-snd-playing? sounds))

;; (n-sounds-playing-now *jack-sounds*)
;; (n-sounds-pausing-now *jack-sounds*)
;; (setf (jack-snd-playing? (first *jack-sounds*)) t)

(defun jackplay-toggle-read (&optional sound (val nil val-provided-p))
  ;; toggles gate on read-from-disk-threads:
  (cond (sound (if val-provided-p
		   (setf (jack-snd-playing? sound) val)
		   (setf (jack-snd-playing? sound) (not (jack-snd-playing? sound))))
	       sound)
	(t (if val-provided-p	;toggle all sounds
	       (setf *jack-reading* val)
	       (setf *jack-reading* (not *jack-reading*)))
	   (if *jack-reading*
	       :reading
	       :pausing))))

;; (jackplay-toggle-read (first *jack-sounds*))
;; (jackplay-toggle-read)

(defun jack-sounds-playing-now (sounds)
  (loop for snd in sounds
       when (jack-snd-playing? snd)
       collect snd))

(defun disk-to-ringbuffer-proc (jack-snd)
  ;;(declare (optimize (float 0) (speed 3)))
  (let ((ringbuffer (jack-snd-ringbuffer jack-snd))
	(sf-handle (jack-snd-sound-file-handle jack-snd)))
    (sb-alien:with-alien ((framebuf jack-ringbuffer-data))
      (loop
	 (let ((sf-playing? (jack-snd-playing? jack-snd))
	       (bytes-per-frame (* +sample-size+ (jack-snd-chans jack-snd))))
	   (when sf-playing?
	     (let ((read-frames-cnt 0))
	       (jack-ringbuffer-get-write-vector ringbuffer (sb-alien:addr framebuf))
	       ;; fill 1st part of available ringbuffer
	       (when (rb-data-len-p framebuf 0)
		 (let ((buf-available (floor (rb-data-len framebuf 0) bytes-per-frame)))
		   (setf read-frames-cnt
			 (sndfile::sf-readf-float sf-handle (rb-data-buf framebuf 0) buf-available)))

		 ;; fill 2nd part of available ringbuffer if available
		 (when (rb-data-len-p framebuf 1)
		   (let ((buf-available (floor (rb-data-len framebuf 1) bytes-per-frame)))
		     (incf read-frames-cnt
			   (sf-readf-float sf-handle (rb-data-buf framebuf 1) buf-available)))))

	       (when (zerop read-frames-cnt) ;at end: loop or quit
		 (let ((looping (jack-snd-loop? jack-snd)))
		   (if looping
		       (jack-seek sf-handle (or (and (consp looping) (ms->frame (car looping))) 0))
		       t ;;(cl-jack-close-sound jack-snd)
		       )))

	       ;; book-keeping
	       (jack-ringbuffer-write-advance ringbuffer (* read-frames-cnt bytes-per-frame))

	       (setf (jack-snd-poker jack-snd) nil)))
	     ;; wait for process-callback to poke me
	   (make-thread #'(lambda () (and sf-playing? (jack-snd-poker jack-snd)))
                        :name (format nil "jack diskin ~:[pausing~;reading~]"
				      (and sf-playing? *jack-reading*))))))))
			

(defun read-from-ringbuffer-to-outbufs (rb nframes in-channels outbus)
  (let ((buf (foreign-alloc 'jack::jack-default-audio-sample-t :count (* nframes in-channels)))
	read-count)
    (setf read-count (jack-ringbuffer-read rb buf (* nframes +sample-size+ in-channels)))
    (list read-count buf in-channels outbus)))
