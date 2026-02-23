;;; media.lisp --- CLI Media Tools

;; 

;;; Code:
(in-package :cli/tools/media)

(define-cli-tool :flamegraph.pl (args &optional (input *standard-input*) (output *standard-output*))
  (let ((proc (sb-ext:run-program *flamegraph.pl* args :wait t :output output :input input)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (flamegraph.pl-error "FLAMEGRAPH.PL command failed: ~A ~A" *flamegraph.pl* (or args "")))))

(defun flamegraph (input &optional output)
  (with-open-file (i input)
    (with-open-stream (o (if output
                             (open output :direction :output :if-exists :supersede :if-does-not-exist :create)
                             (make-string-output-stream)))
      (run-flamegraph.pl nil i o)
      (unless output
        (deserialize (get-output-stream-string o) :svg)))))

(define-cli-tool :ffmpeg (args &optional (output *standard-output*))
  (let ((proc (sb-ext:run-program *ffmpeg* args :wait t :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (ffmpeg-error "FFMPEG command failed: ~A ~A" *ffmpeg* (or args "")))))

#|
 D..... = Decoding supported
 .E.... = Encoding supported
 ..V... = Video codec
 ..A... = Audio codec
 ..S... = Subtitle codec
 ..D... = Data codec
 ..T... = Attachment codec
 ...I.. = Intra frame-only codec
 ....L. = Lossy compression
 .....S = Lossless compression
|#
(define-bitfield ffmpeg-codec-props
  (decode boolean)
  (encode boolean)
  (type (member :video :audio :subtitle :data :attachment))
  (intra boolean)
  (lossy boolean)
  (lossless boolean))

(defun list-ffmpeg-codec-props (i)
  (list
   :decode (ffmpeg-codec-props-decode i)
   :encode (ffmpeg-codec-props-encode i)
   :type (ffmpeg-codec-props-type i)
   :intra (ffmpeg-codec-props-intra i)
   :lossy (ffmpeg-codec-props-lossy i)
   :lossless (ffmpeg-codec-props-lossless i)))

(defun parse-ffmpeg-codec-type (char)
  (ecase char
    (#\V :video)
    (#\A :audio)
    (#\S :subtitle)
    (#\D :data)
    (#\T :attachment)))

(defun parse-ffmpeg-codec-props (str)
  "DEVILS"
  (make-ffmpeg-codec-props
   :decode (char= #\D (schar str 0))
   :encode (char= #\E (schar str 1))
   :type (parse-ffmpeg-codec-type (schar str 2))
   :intra (char= #\I (schar str 3))
   :lossy (char= #\L (schar str 4))
   :lossless (char= #\S (schar str 5))))

(defstruct ffmpeg-codec (props 0 :type ffmpeg-codec-props) name description)

(defmethod print-object ((self ffmpeg-codec) stream)
  (format stream "#S(~A ~A ~{~S~^ ~})"
          (type-of self)
          (ffmpeg-codec-name self)
          (list-ffmpeg-codec-props (ffmpeg-codec-props self))))

(defun read-ffmpeg-codec (stream)
  (when-let ((props (string (read stream nil nil)))
             (name (read stream nil nil))
             (description (trim (read-line stream nil nil))))
    (make-ffmpeg-codec :props (parse-ffmpeg-codec-props props) :name name :description description)))

(defun list-ffmpeg-codecs ()
  (let ((ret (with-output-to-string (s)
               (run-ffmpeg (list "-v" "0" "-codecs") s))))
    (when-let ((i (search " -------" ret)))
      (with-input-from-string (s (subseq ret (+ i 9)))
        (loop for f = (print (read-ffmpeg-codec s))
              while f
              collect f)))))

(defstruct ffmpeg-format props name description)

(defmethod print-object ((self ffmpeg-format) stream)
  (format stream "#S(~A ~A ~{~S~^ ~})"
	  (type-of self)
	  (ffmpeg-format-name self)
	  (list-ffmpeg-format-props (ffmpeg-format-props self))))

(define-bitfield ffmpeg-format-props
  (mux boolean)
  (demux boolean)
  (device boolean))

(defun list-ffmpeg-format-props (i)
  (list
   :mux (ffmpeg-format-props-mux i)
   :demux (ffmpeg-format-props-demux i)
   :device (ffmpeg-format-props-device i)))

(defun parse-ffmpeg-format-props (str)
  (make-ffmpeg-format-props
   :demux (find #\D str)
   :mux (find #\E str)
   :device (find #\d str)))

(defun read-ffmpeg-format (stream)
  (when-let ((props (string (read stream nil nil)))
	     (name (read stream nil nil))
	     (description (trim (read-line stream nil nil))))
    (make-ffmpeg-format :props (parse-ffmpeg-format-props props) :name name :description description)))
  
(defun list-ffmpeg-formats ()
  (let ((ret (with-output-to-string (s)
               (run-ffmpeg (list "-v" "0" "-formats") s))))
    (when-let ((i (search " ---" ret)))
      (with-input-from-string (s (subseq ret (+ i 5)))
        (loop for f = (print (read-ffmpeg-format s))
              while f
              collect f)))))

(define-cli-tool :mpv (&rest args)
  (let ((proc (sb-ext:run-program *mpv* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (mpv-error "MPV command failed: ~A ~A" *mpv* (or args "")))))

(defvar *mpv-config-path* (merge-homedir-pathnames ".config/mpv/mpv.conf"))

;; incomplete config description
(defconfig mpv-config (cli-tool-config ini-document) 
  (fs
   profile
   hwdec
   user-agent
   alang
   slang))

(defmethod make-config ((self (eql :mpv)) &rest args &key path &allow-other-keys)
  (if (remf args :path)
      (load-ast (apply 'change-class (deserialize path :ini) 'mpv-config args))
      (apply 'make-instance 'mpv-config args)))

(defmethod load-ast ((self mpv-config))
  (with-slots (ast) self
    (if (formp ast)
	(mapc
         (lambda (x)
           (let ((k (car x)) (v (cdr x)))
	     (when-let ((s (print (find-symbol* (string-upcase k) #.*package* nil)))) ;; needs to be correct package
	       (unless (null v)
		 (setf v
		       (case k
			 (:fs v)
			 (:hwdec v)
			 (:alang v)
			 (:slang v)
			 (t v)))
		 (setf (slot-value self s) v)))))
         ast)
	;; invalid ast, signal error
	(error 'syntax-error)))
  (unless *keep-ast* (setf (ast self) nil))
  self)
    
(defun load-mpv-config (&optional (path *mpv-config-path*))
  (when (probe-file path)
    (make-config :mpv :path path)))

(define-cli-tool :wireplumber (&rest args)
  (let ((proc (sb-ext:run-program *wireplumber* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (wireplumber-error "WIREPLUMBER command failed: ~A ~A" *wireplumber* (or args "")))))

(define-cli-tool :picard (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *picard* args :wait wait :output output)))
    (unless (positive-integer-p #1=(sb-ext:process-exit-code proc))
      (picard-error "PICARD command failed: ~A ~{~A ~^~}~%exit-code = ~A" *picard* (or args "") #1#))))

(defvar *picard-config-path* (merge-homedir-pathnames ".config/MusicBrainz/Picard.ini"))

(defconfig picard-config (cli-tool-config ini-document) ())

(defmethod make-config ((self (eql :picard)) &rest args &key path &allow-other-keys)
  (if (remf args :path)
      (apply 'change-class (deserialize path :ini) 'picard-config args)
      (apply 'make-instance 'picard-config args)))

(defun load-picard-config (&optional (path *picard-config-path*))
  (when (probe-file path)
    (make-config :picard :path path)))

(defvar *picard-commands*
  '(:clear-logs
    :cluster
    :fingerprint
    :from-file
    :load
    :lookup
    :lookup-cd
    :pause
    :quit
    :remove
    :remove-all
    :remove-empty
    :remove-saved
    :save-matched
    :scan
    :show
    :submit-fingerprints
    :write-logs))

(eval-always
  (defun picard-cmd (cmd)
    (when (member cmd *picard-commands*)
      (substitute #\_ #\- (string-upcase cmd)))))

;; TODO 2025-04-05: 
(defun %do-picard (body)
  "Execute a sequence of forms where atoms are interpreted by picard as commands
or arguments and lists are evaluated  interpreted as args."
  (let ((cmd) (args) (ret))
    (loop for i below (length body)
          for a in body
          do (typecase a
               (symbol (if cmd
                           (progn
                             (push (cons cmd args) ret)
                             (setf cmd a
                                   args nil))
                           (setf cmd a)))
               (t (push (format nil "~A" a) args)))
          finally 
             (progn
               (when cmd (push (cons cmd args) ret))
               (return (nreverse ret))))))

(defun exec-picard (&rest commands)
  "Execute a PICARD batch program consisting of COMMANDS."
  (run-picard (flatten (mapcar (lambda (x) (cons "-e" (rplaca x (string (car x))))) commands))))

(defmacro do-picard (&body body)
  `(progn
     (apply 'exec-picard ',(%do-picard body))))
