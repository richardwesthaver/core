;;; pkg.lisp --- JACK Audio Connection Kit FFI

;; 

;;; Code:
(defpackage :jack
  (:use :cl :std :log :sb-alien)
  (:export :load-jack))

(in-package :jack)

(define-alien-loader :jack "/usr/lib/")

(define-alien-routine jack-get-version-string c-string)

(define-alien-type jack-nframes-t unsigned-int)
(define-alien-type jack-port-t (* t))
(define-alien-type jack-options-t (* t))
(define-alien-type jack-time-t unsigned-long)
(define-alien-type jack-midi-data-t unsigned-char)

(defvar *jack-default-audio-type* "32 bit float mono audio")
(defvar *jack-default-midi-type* "8 bit raw midi")
(define-alien-type jack-default-audio-sample-t float)

(define-alien-enum (jackoptions int)
  :null #x00
  :no-start-server #x01
  :use-exact-name #x02
  :server-name #x04
  :load-name #x08
  :load-init #x10
  :session-id #x20)

(define-alien-enum (jackportflags int)
  :is-input #x1
  :is-output #x2
  :is-physical #x4
  :can-monitor #x8
  :is-terminal #x10)

(define-alien-routine jack-client-name-size int)
(define-alien-routine input-port jack-port-t)
(define-alien-routine jack-client-open (* t)
  (name c-string)
  (opt int)
  (status int))

(define-alien-routine jack-get-sample-rate int
  (client (* t)))

(define-alien-routine jack-port-type-get-buffer-size size-t
  (client (* t))
  (port-type c-string))

(define-alien-routine jack-get-buffer-size jack-nframes-t
  (client (* t)))

(define-alien-routine jack-get-client-name c-string
  (client (* t)))

(define-alien-routine jack-port-get-buffer (* t)
  (port (* t))
  (frames jack-nframes-t))

(define-alien-routine jack-port-name c-string
  (port (* jack-port-t)))

(define-alien-routine jack-connect int
  (client (* t))
  (source-port c-string)
  (destination-port c-string))

(define-alien-routine jack-disconnect int
  (client (* t))
  (source-port c-string)
  (destination-port c-string))

(define-alien-routine jack-get-ports (* t)
  (client (* t))
  (port_name_pattern c-string)
  (type_name_pattern c-string)
  (flags unsigned-long))

(define-alien-routine jack-port-register (* t)
  (client (* t))
  (port-name c-string)
  (port-type c-string)
  (flags unsigned-long)
  (buffer-size unsigned-long))

(define-alien-routine jack-client-close int
  (client (* t)))

(define-alien-routine jack-activate int
  (client (* t)))

(define-alien-routine jack-deactivate int
  (client (* t)))

(define-alien-routine jack-set-process-callback int
  (client (* t))
  (process_callback (* t))
  (arg int))

(define-alien-routine jack-midi-clear-buffer void
  (port-buffer (* t)))

(define-alien-routine jack-midi-event-reserve (* t)
  (port-buffer (* t))
  (time unsigned-int)
  (data-size unsigned-char))

;;; TIME
(define-alien-routine jack-get-time jack-time-t)

(define-alien-routine jack-frames-to-time jack-time-t
  (client (* t))
  (frames jack-nframes-t))

(define-alien-routine jack-time-to-frames jack-nframes-t
  (client (* t))
  (time jack-time-t))

(define-alien-routine jack-last-frame-time jack-nframes-t
  (client (* t)))

(define-alien-routine jack-frame-time jack-nframes-t
  (client (* t)))

(define-alien-type jack-ringbuffer
    (struct jack-ringbuffer-t
      (buf (* char))
      (write-ptr size-t)
      (read-ptr size-t)
      (size size-t)
      (size-mask size-t)
      (mlocked int)))

(define-alien-type jack-ringbuffer-data
    (struct jack-ringbuffer-data-t
      (buf (* float))
      (len size-t)))

;; vec[0].buf
(defun rb-data-buf (arr index)		;index is 0 or 1 from jack
  (sb-alien:slot
   (sb-alien:deref (sb-alien:cast arr (* jack-ringbuffer-data)) index)
   'buf))

;;vec[0].len
(defun rb-data-len (arr index)
  (sb-alien:slot
   (sb-alien:deref (sb-alien:cast arr (* jack-ringbuffer-data)) index)
   'len))

;;(rb-data-len vec 0)
(defun rb-data-len-p (arr index)	;len=0 := nothing to get
  (plusp (rb-data-len arr index)))

(define-alien-routine jack-ringbuffer-create (* jack-ringbuffer)
  (sz size-t))

(define-alien-routine jack-ringbuffer-reset void
  (rb (* jack-ringbuffer)))

(define-alien-routine jack-ringbuffer-get-write-vector void
  (rb (* jack-ringbuffer))
  (vec (* jack-ringbuffer-data)))

(define-alien-routine jack-ringbuffer-free void
  (rb (* jack-ringbuffer)))

(define-alien-routine jack-ringbuffer-write-advance void
  (rb (* jack-ringbuffer))
  (cnt size-t))

(define-alien-routine jack-ringbuffer-write-space size-t
  (rb (* jack-ringbuffer)))

(define-alien-routine jack-ringbuffer-write size-t
  (rb (* jack-ringbuffer))
  (src (* char))
  (cnt size-t))

(define-alien-routine jack-ringbuffer-get-read-vector void
  (rb (* jack-ringbuffer))
  (vec (* jack-ringbuffer-data)))

(define-alien-routine jack-ringbuffer-read size-t
  (rb (* jack-ringbuffer))
  (dest (* char))
  (cnt size-t))

(define-alien-routine jack-ringbuffer-read-space size-t
  (rb (* jack-ringbuffer)))

;;; MIDI
(defvar *jack-midi-output-port* nil)
(defvar *jack-midi-input-port* nil)

;;; global pool of seqs for this client, for separate control [start/stop/pause...]:
(defun make-jack-seqs () (make-hash-table :size 1500
					  :rehash-size 1.5
					  :rehash-threshold 0.7
					 )) 
(defparameter *jack-seqs* (make-jack-seqs))

;;; event-seq is a hash-table, keys are frameno at jacks' start-of-period (ie: jack-last-frame-time)
(defun make-jack-seq () (make-hash-table))

;;; provide one default seq for global queues, external schedulers etc:
 ;default sequencer
(defvar *jack-seq*
  (setf (gethash '*jack-seq* *jack-seqs*) (make-jack-seq)))

;;; MIDI EVENTS

;; TODO 2025-03-25: use dat/midi
;; TODO: expand with support for all midi-messages

(defun jack-add-event-this-period (seq period event)
  (setf (gethash period seq)
	(sort (nconc (gethash period seq) (list event))
	      #'(lambda (a b) (< (car a) (car b))))))

(defun jack-add-event-this-frame (seq frame event)
  (push event (gethash frame seq)))

;;; SEQUENCING EVENTS

;; seq is a hashtable, key'ing on frame-numbers

;; version hashing on frame-number
(defun seqhash-midi-event (seq frame event)
  (jack-add-event-this-frame seq frame event))

;;; using midi-classes:

(defun seqhash-midi-note-on (seq frame noteno velocity &optional (channel 1))
  (let ((event (om-midi::make-note-on-message frame noteno velocity channel)))
    (seqhash-midi-event seq frame event)))

(defun seqhash-midi-note-off (seq frame noteno velocity &optional (channel 1))
  (let ((event (om-midi::make-note-off-message frame noteno velocity channel)))
    (seqhash-midi-event seq frame event)))

(defun seqhash-midi-program-change (seq frame program &optional (channel 1))
  (let ((event (om-midi::make-program-change-message frame program channel)))
    (seqhash-midi-event seq frame event)))

(defun seqhash-midi-control-change (seq frame control value &optional (channel 1))
  (let ((event (om-midi::make-control-change-message frame control value channel)))
    (seqhash-midi-event seq frame event)))

(defun seqhash-midi-pitch-wheel-msg (seq frame bend &optional (channel 1))
  (let ((mybend (+ bend 8192)))		;expects values between -8192->8191
    (let ((event (om-midi::make-pitch-bend-message frame bend channel)))   ;;; use bend: in OM 6.9 values are 0-16383
      (seqhash-midi-event seq frame event))))

;; erase pending note-offs for interval - don't shut off later arriving notes
(defun seqhash-clear-note-offs (seq startframe endframe noteno &optional (channel 1))
  (maphash #'(lambda (key val)
	       (let ((event (car val)))
		 (when (and (<= startframe key endframe)
			    (typep event 'midi::note-off-message)
			    (eql (om-midi::midi-key event) noteno)
			    (eql (om-midi::midi-channel event) channel))
		   (remhash key seq))))
	   seq))

;; interface to higher-level funcs:

(defun jack-start-dur-to-frames (start dur)
  (let* ((dur-frames (sec->frame dur))
	 (startframe (jack-frame-now start))
	 (endframe (+ startframe dur-frames -1)))
    (values startframe endframe)))

(defun jack-play-event (seq start event)
  (seqhash-midi-event seq (jack-frame-now start) event))

(defun jack-play-note (seq start dur noteno &optional (vel 80) (chan 0))
  (let* ((startframe (jack-frame-now start))
	 (endframe (+ startframe (sec->frame dur) -1)))
    (seqhash-clear-note-offs seq startframe endframe noteno chan)
    (seqhash-midi-note-on seq startframe noteno vel chan)
    ;; (sleep (/ (jack-get-buffer-size *CLJackClient*)
    ;; 	   (jack-get-sample-rate *CLJackClient*)))
    (seqhash-midi-note-off seq endframe noteno 0 chan)))

(defun jack-all-notes-off (seq)
  (let ((sounding-notes '()))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (mapc #'(lambda (ev) (push (list (om-midi::midi-key ev) (1- (om-midi::midi-channel ev)))
					    sounding-notes))
		       val))
	     seq)
    (clrhash seq)
    (mapc #'(lambda (note)
	      (seqhash-midi-note-off seq (jack-frame-now) (car note) 0 (cadr note)))
	  sounding-notes)))

(defun jack-all-notes-off-and-kill-seq (seq)
  (jack-all-notes-off seq)
  (sleep (float (/ 2
  		   (jack-get-buffer-size *CLJACKCLIENT*)
  		   (jack-get-sample-rate *CLJACKCLIENT*))))
  (remhash seq *jack-seqs*))

(defun jack-reset (&optional (seq *jack-seq*))
  (dotimes (ch 16)
    (dotimes (key 127)
      (seqhash-midi-note-off seq (jack-frame-now) key 0 ch))))

;;(jack-reset)

(defun jack-reset-channels ()
  (loop for ch from 0 to 16
     do (seqhash-midi-program-change *jack-seq* (jack-frame-now) ch ch)))

;;(jack-reset-channels)

(defun jack-seq-hush-this-seq (seq)
  (jack-all-notes-off seq))

(defun jack-seq-hush-all-seqs ()
  (maphash #'(lambda (key seq)
	       (declare (ignore key))
	       (jack-all-notes-off-and-kill-seq seq))
	   *jack-seqs*))

(defparameter *jack-playing* t)		;nil=shut up
;; (setf *playing* nil)

(defun play-from-seq (port-buf seq)
  (when *jack-playing*
    (let ((this-period (jack-last-frame-time *jack-client*)))
      (loop for offset from 0 below (jack-get-buffer-size *jack-client*)
	 for key from this-period	;events hashed on frameno
	 for events = (gethash key seq)
	 when events
	 do 
	   (dolist (midimsg events)
	     (let ((buffer (jack-midi-event-reserve port-buf offset 3))) ;offset inside period
	       (unless (null-alien buffer)
		 (setf (mem-aref buffer :int8 0) (om-midi::midi-status-byte midimsg) ;command
		       (mem-aref buffer :int8 1) (om-midi::midi-data-byte-1 midimsg) ;data-byte 1
		       (mem-aref buffer :int8 2) (om-midi::midi-data-byte-2 midimsg) ;data-byte 2
		       ))))
	   (remhash key seq)))))

;; callback function handles seq-events, plugged into jacks
;; process-callback

(defun jack-handle-event-seqs (nframes)
  (let ((port-buf (jack-port-get-buffer *jack-midi-output-port* nframes)))
    (jack-midi-clear-buffer port-buf)
    ;;(play-from-seq port-buf *jack-seq*)
    (maphash #'(lambda (key seq)
		 (declare (ignore key))
		 (play-from-seq port-buf seq))
	     *jack-seqs*)))



(defun jack-init-midi ()

  ;; get up and running

  (unless *jack-client*
    (setf *jack-client* (jack-client-open "lisp-jack" 0 0)))

  (setf *jack-midi-output-port*
	(let ((port (jack-port-register *jack-client*
					"midiout"
					*jack-default-midi-type*
					(jackportflags :is-output)
					0)))
	  (when (zerop (sb-sys:sap-int (alien-sap port))) ;0 if not allocated
	    (setf port -1)
	    (cerror "Set *jack-midi-output-port* to -1" "*jack-midi-output-port* for Jack not allocated - check jack-server"))
	  port)))

;;; Client
;; default global client-name
(defparameter *jack-client* nil)

(defun jack-period-now (&optional sek)
  (+ (jack-last-frame-time *jack-client*)
     (jack-get-buffer-size *jack-client*)
     (round (if sek (* sek (jack-get-sample-rate *jack-client*)) 0))))

;;; too late to schedule things inside current period, this looks up
;;; current frame with exactly one period latency:

(defun jack-frame-now (&optional sek)
  (round (+ (jack-frame-time *jack-client*)
	    (jack-get-buffer-size *jack-client*) 
	    (if sek (* sek (jack-get-sample-rate *jack-client*)) 0))))

(defun ms->frame (ms)
  (round (* ms (jack-get-sample-rate *jack-client*)) 1000))

(defun sec->frame (sec)
  (round (* sec (jack-get-sample-rate *jack-client*))))

(defun frame->period-offset (frame)
  "returns 2 frame nos: start of period & offset within period"
  (let ((bufsiz (jack-get-buffer-size *jack-client*)))
    (multiple-value-bind (n rem)
	(floor frame bufsiz)
      (values (* n bufsiz) rem))))

(defparameter *jack-audio-input-channels* 2)
(defparameter *jack-audio-output-channels* 2)

(defparameter *jack-audio-input-ports* nil)
(defparameter *jack-audio-output-ports* nil)

(defun cl-jack-init-audio ()
  (unless *jack-client*
    (setf *jack-client* (jack-client-open "lisp-jack" 0 0)))
  (setf *jack-audio-input-ports*
	(loop for chan from 0 below *jack-audio-output-channels*
	   collect
	     (let ((port (jack-port-register
			  *jack-client*
			  (format nil "in_~A" chan)
			  *jack-default-audio-type*
			  (jackportflags :is-input)
			  0)))
	       (when (zerop (sb-sys:sap-int (alien-sap port))) ;0 if not allocated
		 (setf port -1)
		 (cerror (format nil "Set jack-input-port ~A to -1" chan)
			 "*jack-audio-input-ports* not allocated"))
	       port)))

  (setf *jack-audio-output-ports*
	(loop for chan from 0 below *jack-audio-output-channels*
	   collect
	     (let ((port (jack-port-register
			  *jack-client*
			  (format nil "out_~A" chan)
			  *jack-default-audio-type*
			  (jackportflags :is-output)
			  0)))
	       (when (zerop (sb-sys:sap-int (alien-sap port))) ;0 if not allocated
		 (setf port -1)
		 (cerror (format nil "Set jack-output-port ~A to -1" chan)
			 "*jack-audio-output-ports* not allocated"))
	       port)))

  ;; provide default-callback which just copies in to out:

  (define-alien-callable jack-process-callback-silence int ((nframes jack-nframes-t) (arg (* t)))
    (declare (ignorable arg))
    (when (fboundp 'jack-handle-event-seqs) (jack-handle-event-seqs nframes))
    (loop for inport in *jack-audio-input-ports*
       for outport in *jack-audio-output-ports*
       do
	 (let ((in (jack-port-get-buffer inport nframes))
	       (out (jack-port-get-buffer outport nframes)))
	   (memcpy out in
		   (* nframes (std/alien::foreign-type-size 'size-t)))))
  
    0)
  ;;(jack-deactivate *CLJackClient*)
  )

(defun jack-connect-audio-client-to-system-output ()
  (loop for port in *jack-audio-output-ports*
        for system-out from 1
        do (or (not  (minusp (jack-connect *jack-client*
					   (jack-port-name port)
					   (format nil "system:playback_~A" system-out))))
	       (warn "could not connect CLJack port ~A to output-port ~A" (jack-port-name port) system-out))))
