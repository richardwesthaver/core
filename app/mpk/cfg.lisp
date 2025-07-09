;;; cfg.lisp --- MPK Config

;; 

;;; Code:
(in-package :mpk)

(defconfig mpk-config (ast id)
  ((path :initform nil :initarg :path :type (or pathname null))
   (logger :initform (default-logger-config) :initarg :logger :type (or null logger-config) :accessor logger)
   (mpd :initarg :mpd :type mpd:mpd-config :initform (mpd::load-mpd-config))
   (audio :initarg :audio :type audio-system-config) ;; alsa/pipewire/jack
   (video :initarg :video :type video-system-config)
   (metro :initarg :metro :type mpk/metro:metro-config)
   (db :initarg :db :type mpk/db:mpk-db-config)
   (picard :initarg :picard :type cli/tools/media:picard-config :initform (cli/tools/media:load-picard-config))
   (transmission :initarg :transmission :type cli/tools/net::transmission-config :initform (cli/tools/net::load-transmission-config))
   (ytdl :initarg :ytdl :type cli/tools/net::ytdl-config :initform (cli/tools/net::load-ytdl-config))))

(defmethod make-config ((self (eql :mpk)) &rest args &key &allow-other-keys)
  (apply 'make-instance 'mpk-config args))

;; obj -> ast
(defmethod build-ast ((self mpk-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast:ast self)
	(ast:unwrap-object self
		       :slots t
		       :methods nil
		       :nullp nullp
		       :exclude exclude)))

;; ast -> obj
(defmethod load-ast ((self mpk-config))
  (with-slots (ast) self
    (if (formp ast)
	(progn
	  (sb-int:doplist (k v) ast
	    (when-let ((s (find-mpk-symbol k))) ;; needs to be correct package
	      (unless (null v)
		(setf v
		      (case k
			(:logger (apply 'make-config :logger v))
			(:mpd (apply 'make-instance 'mpk/mpd:mpd-config v))
                        (:picard (apply 'make-instance 'picard-config v))
                        (:transmission (apply 'make-config :transmission v))
                        (:ytdl (make-instance 'ytdl-config :ast v))
			(t v)))
		(setf (slot-value self s) v))))
	  (unless *keep-ast* (setf (ast self) nil))
	  self)
	;; invalid ast, signal error
	(error 'syntax-error))))
        
(defun init-mpkrc (&optional (file *user-mpkrc*))
  (let ((cfg (make-instance 'mpk-config)))
    (build-ast cfg :exclude '(ast id logger))
    (with-open-file (out file 
                         :direction :output
                         :if-does-not-exist :create)
      (write-ast cfg out :fmt :canonical))))

(defun load-mpkrc (&optional (file *user-mpkrc*) (init t))
  "Load a mpkrc configuration from FILE. Defaults to ~/.mpkrc."
  (flet ((%load ()
	   (with-readtable :shell
	     (let ((form (sxp:file-read-forms file)))
	       (setq *mpk-user-config*
                     (load-ast (make-instance 'mpk-config :ast form :path file :id (sxhash form))))))))
    (if (not init)
	(progn 
	  (assert (probe-file file))
	  (%load))
	(if (probe-file file)
	    (%load)
	    (init-mpkrc file))))
  (setf *log-level* (level (logger *mpk-user-config*)))
  *mpk-user-config*)
