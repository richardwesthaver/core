;;; aud/mpd.lisp --- MPD Interface for Lisp

;; based on https://github.com/stassats/mpd

;;; Commentary:

;; The original code hasn't been updated in quite some time. Here
;; we've added in some missing slots, fixed a typo, removed the
;; dependency on usocket library and extended the functionality
;; slightly.


;;; Code:
(in-package :aud/mpd)
;;; Classes
(define-condition mpd-error (error)
  ((text :initarg :text :reader text
         :initform nil))
  (:report (lambda (condition stream)
             (princ (text condition) stream))))

(macrolet ((define-conditions (names)
             `(progn ,@(mapcar
                        (lambda (name)
                          `(define-condition ,name (mpd-error) ()))
                        names))))
  (define-conditions (bad-argument incorrect-password
                      not-permitted unknown-command not-exist
                      playlist-size-exceed already-updating exist)))

(defparameter *error-ids-alist*
  '((2 . bad-argument)
    (3 . incorrect-password)
    (4 . not-permitted)
    (5 . unknown-command)
    (50 . not-exist)
    (51 . playlist-size-exceed)
    (54 . already-updating)
    (56 . exist)))

(eval-always
  (defparameter *tag-types*
    '(:artist :album :title :track :name :genre :date
      :composer :performer :comment :disc :filename :any)
    "Types of tags for using in `search' and `find'"))

(deftype tag-type ()
  `(member ,@*tag-types*))

(defclass track ()
  ((file
    :initform nil :initarg :file :accessor file)
   (title
    :initform nil :initarg :title :accessor title)
   (artist
    :initform nil :initarg :artist :accessor artist)
   (albumartist
    :initform nil :initarg :albumartist :accessor albumartist)
   (album
    :initform nil :initarg :album :accessor album)
   (genre
    :initform nil :initarg :genre :accessor genre)
   (date
    :initform nil :initarg :date :accessor date)
   (performer
    :initform nil :initarg :performer :accessor performer)
   (composer
    :initform nil :initarg :composer :accessor composer)
   (disc
    :initform nil :initarg :disc :accessor disc)
   (track
    :initform nil :initarg :track :accessor track-number)
   (time
    :initform nil :initarg :time :accessor duration)
   (last-modified
    :initform nil :initarg :last-modified :accessor last-modified)))

(defclass playlist (track)
  ((pos
    :initform 0 :initarg :pos :accessor position-in-playlist
    :type integer)
   (duration
    :initform nil :initarg :duration)
   (format :initform nil :initarg :format)
   (id
    :initform 0 :initarg :id :accessor id
    :type integer)))

(defclass status ()
  ((volume
    :reader volume :initarg :volume :initform nil)
   (repeat
    :reader repeat :initarg :repeat :initform nil)
   (random
    :reader randomized :initarg :random :initform nil)
   (playlist
    :reader playlist-version :initarg :playlist :initform nil)
   (playlist-length
    :reader playlist-length :initarg :playlistlength :initform nil)
   (xfade
    :reader xfade :initarg :xfade :initform nil)
   (state
    :reader state :initarg :state :initform nil)
   (partition
    :reader partition :initarg :partition :initform nil)
   (audio
    :reader audio :initarg :audio :initform nil)
   (bitrate
    :reader bitrate :initarg :bitrate :initform nil)
   (duration
    :reader duration :initarg :duration :initform nil)
   (time
    :reader %time :initarg :time :initform nil)
   (songid
    :reader songid :initarg :songid :initform nil)
   (song :reader song :initarg :song :initform nil)
   (nextsongid
    :reader nextsongid :initarg :nextsongid :initform nil)
   (nextsong 
    :reader nextsong :initarg :nextsong :initform nil)
   (elapsed
    :reader elapsed :initarg :elapsed :initform nil)
   (mixrampdb 
    :reader mixrampdb :initarg :mixrampdb :initform nil)
   (consume 
    :reader consume :initarg :consume :initform nil)
   (single 
    :reader single :initarg :single :initform nil)))

(defclass stats ()
  ((artists
    :reader artists :initarg :artists :initform nil)
   (albums
    :reader albums :initarg :albums :initform nil)
   (songs
    :reader songs :initarg :songs :initform nil)
   (uptime
    :reader uptime :initarg :uptime :initform nil)
   (playtime
    :reader playtime :initarg :playtime :initform nil)
   (db-playtime
    :reader db-playtime :initarg :db_playtime :initform nil)
   (db-update
    :reader db-update :initarg :db_update :initform nil)))

(macrolet ((generate-commands (class names)
             `(progn
                ,@(mapcar (lambda (name)
                            `(defmethod ,name ((stream socket))
                               (,name (,class stream))))
                          names))))
  (generate-commands status
                     (volume repeat randomized playlist-version playlist-length
                      xfade state audio bitrate duration songid song))
  (generate-commands stats
                     (artists albums songs uptime playtime db-playtime db-update)))

(defparameter *integer-keys*
  '(:id :pos :volume :playlist :playlistlength
    :xfade :song :songid :bitrate :playtime
    :artists :albums :songs :uptime :db_playtime :db_update
    :outputid)
  "List of keys which values must be integers.")

(defparameter *value-processing-functions*
  '(:time parse-time :state to-keyword
    :random string-not-zerop :repeat string-not-zerop
    :outputenabled string-not-zerop))

(defmethod print-object ((object track) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (artist title album) object
      (format stream "~A - ~A (~A)" artist title album))))

;;; MPD
(defvar *default-host* "localhost")
(defvar *default-port* 6600)

(defun connect (&key (host *default-host*) (port *default-port*) password)
  "Connect to MPD."
  (let ((connection (socket-connect (make-instance 'inet-socket :type :stream) (get-address-by-name host) port)))
    (prog1 (values connection
                   (read-answer (socket-make-stream connection :input t :output t)))
      (when password
        (password connection password)))))

(defun read-answer (stream)
  (loop for line = (read-line stream)
        until (string= line "OK" :end1 2)
        collect line
        when (string= line "ACK" :end1 3)
        do (throw-error line)))

(defun throw-error (text)
  ;; Error format: `ACK [<error id>@<position>] {<comand name>} <description>'
  (let* ((error-id (parse-integer text :start 5 :junk-allowed t))
         (delimiter (position #\] text))
         (condition (cdr (assoc error-id *error-ids-alist*))))
    (error condition :text (subseq text (+ delimiter 2)))))

(eval-always
  (defmacro with-mpc ((var &rest options) &body body)
    `(let ((,var (connect ,@options)))
       (unwind-protect
            (progn ,@body)
         (disconnect ,var)))))

(defun ensure-mpd ()
  (handler-case
      (with-mpc (c) t)
    (not-exist () (sb-ext:run-program "mpd" nil :search t :directory (user-homedir-pathname) :wait nil))))

(defun send-command (connection command)
  "Send command to MPD."
  (let ((stream (socket-make-stream connection :input t)))
    (unless (open-stream-p stream)
      (error 'mpd-error :text (format nil "The stream ~A is not opened." stream)))
    (write-line command stream)
    (finish-output stream)
    (read-answer stream)))

;;; Parsing

(defun to-keyword (name)
  (intern (string-upcase name) :keyword))

(defun split-value (string)
  "Split a string `key: value' into (list :key value)."
  (let ((column (position #\: string)))
    (process-value (to-keyword (subseq string 0 column))
                   (subseq string (+ 2 column)))))

(defun split-values (strings)
  "Transform a list of strings 'key: value' into the plist."
  (mapcan #'split-value strings))

(defun process-value (key value)
  (list key
        (funcall (value-processing-function key) value)))

(defun value-processing-function (key)
  (if (member key *integer-keys*)
      #'parse-integer
      (getf *value-processing-functions* key #'identity)))

(defun parse-time (time)
  "\"10:20\" -> (10 20); \"10\" -> 10"
  (multiple-value-bind (first stop)
      (parse-integer time :junk-allowed t)
    (if (= stop (length time))
        first
        (list first
              (parse-integer time :start (1+ stop))))))

(defun string-not-zerop (string)
  (not (string= string "0")))

(defun filter-keys (strings)
  "Transform a list of strings 'key: value' into a list of values."
  (mapcar (lambda (entry)
            (subseq entry (+ 2 (position #\: entry))))
          strings))

(defun make-class (data type)
  "Make a new instance of the class playlist with initargs from
   the list of strings `key: value'."
  (apply 'make-instance type (split-values data)))

(defun parse-list (list &optional class)
  "Make a list of new instances of the class `class' with initargs from
   a list of strings `key: value'. Each track is separeted by the `file' key."
  (let (track)
    (flet ((create-track ()
             (when track
               (list (apply 'make-instance class track)))))
      (nconc
       (mapcan (lambda (x)
                 (let ((pair (split-value x)))
                   (case (car pair)
                     (:file (prog1 (create-track)
                              (setf track pair)))
                     ((:directory :playlist)
                      (list pair))
                     (t (nconc track pair)
                        nil))))
               list)
       (create-track)))))

;;;

(defun process-string (string)
  "Check for emtpy strings, and escape strings when needed."
  (when string
    (let ((string
           (string-trim '(#\Space #\Tab #\Newline) string)))
      (when (zerop (length string))
        (error 'mpd-error :text "Zero length argument."))
      (if (position #\Space string)
          (prin1-to-string string)
          string))))

;;; Macros

(defmacro send (&rest commands)
  "Macro for using inside `defcommand'."
  `(send-command connection
                 (format nil "~{~A~^ ~}"
                         (remove nil (list ,@commands)))))

(defmacro defcommand (name parameters &body body)
  `(defun ,name (connection ,@parameters)
     ,@body))

(defmacro defmethod-command (name parameters &body body)
  `(defmethod ,name (connection ,@parameters)
     ,@body))

(defmacro check-args (type &rest args)
  "Check string and integer arguments."
  (if (or (eq type 'string)
          (and (listp type)
               (member 'string type)))
      `(progn ,@(mapcan
                 (lambda (arg)
                   `((check-type ,arg ,type "a string")
                     (setf ,arg (process-string ,arg))))
                 args))
      `(progn ,@(mapcar
                 (lambda (arg)
                   `(check-type ,arg ,type))
                 args))))

;;; Commands
(defcommand password (password)
  "Authentication."
  (check-args string password)
  (send "password" password))

(defcommand disconnect ()
  "Close connection."
  (socket-close connection))

(defcommand now-playing ()
  "Return instance of playlist with current song."
  (let ((track (send "currentsong")))
    (when track
      (make-class track 'playlist))))

(defcommand disable-output (id)
  (check-args unsigned-byte id)
  (send "disableoutput" id))

(defcommand enable-output (id)
  (check-args unsigned-byte id)
  (send "enableoutput" id))

(defcommand ping ()
  "Send ping to MPD."
  (send "ping"))

(defcommand kill ()
  "Stop MPD in a safe way."
  (send "kill"))

(defcommand status ()
  "Return status of MPD."
  (make-class (send "status") 'status))

(defcommand stats ()
  "Return statisics."
  (make-class (send "stats") 'stats))

(defcommand outputs ()
  "Return information about all outputs."
  (split-values (send "outputs")))

(defcommand commands ()
  "Return list of available commands."
  (filter-keys (send "commands")))

(defcommand not-commands ()
  "Return list of commands to which the current user does not have access."
  (filter-keys
   (send "notcommands")))

;;; Control

(defcommand pause ()
  "Toggle pause / resume playing."
  (send "pause"))

(defcommand play (&optional song-number)
  (check-args (or unsigned-byte null) song-number)
  "Begin playing the playlist starting from song-number, default is 0."
  (send "play" song-number))

(defcommand stop ()
  "Stop playing."
  (send "stop"))

(defcommand next ()
  "Play next track in the playlist."
  (send "next"))

(defcommand previous ()
  "Play previous track in the playlist."
  (send "previous"))

(defcommand crossfade (seconds)
  (check-args unsigned-byte seconds)
  "Sets crossfading between songs."
  (send "crossfade" seconds))

;; Playlist

(defcommand list-playlist (name)
  "List files in the playlist `name'"
  (check-args string name)
  (filter-keys (send "listplaylist" name)))

(defcommand list-playlist-info (name)
  "List metadata of tracks in the playlist `name'"
  (check-args string name)
  (parse-list (send "listplaylistinfo" name) 'playlist))

(defcommand clear ()
  "Clear the current playlist."
  (send "clear"))

(defcommand save-playlist (filename)
  "Save the current playlist to the file in the playlist directory."
  (check-args string filename)
  (send "save" filename))

(defcommand load-playlist (filename)
  "Load playlist from file."
  (check-args string filename)
  (send "load" filename))

(defcommand rename-playlist (name new-name)
  "Rename playlist."
  (check-args string name new-name)
  (unless (equal name new-name)
    (send "rename" name new-name)))

(defcommand playlist-info (&optional id)
  "Return content of the current playlist."
  (check-args (or unsigned-byte null) id)
  (if id
      (make-class (send "playlistinfo" id) 'playlist)
      (parse-list (send "playlistinfo") 'playlist)))

(defcommand playlist-changes (version)
  "Return changed songs currently in the playlist since `version'."
  (check-args unsigned-byte version)
  (parse-list (send "plchanges" version) 'playlist))

(defcommand add-to-playlist (name path)
  "Add `path' to the playlist `name'."
  (check-args string name path)
  (send "playlistadd" name path))

(defcommand clear-playlist (name)
  "Clear playlist `name'."
  (check-args string name)
  (send "playlistclear"))

(defcommand delete-from-playlist (name song-id)
  "Delete `song-id' from playlist `name'."
  (check-args string name)
  (check-args unsigned-byte song-id)
  (send "playlistdelete" name song-id))

(defcommand move-in-playlist (name song-id position)
  "Move `song-id' in playlist `name' to `position'."
  (check-args string name)
  (check-args unsigned-byte song-id position)
  (send "playlistmove" name song-id position))

(defcommand find-in-current-playlist (scope query)
  "Search for songs in the current playlist with strict matching."
  (check-args string scope query)
  (send "playlistfind" scope query))

(defcommand search-in-current-playlist (scope query)
  "Search case-insensitively with partial matches for songs in the current playlist"
  (check-args string scope query)
  (send "playlistsearch" scope query))

(defgeneric add (connection what)
  (:documentation "Add file or directory to the current playlist."))

(defmethod-command add ((what track))
  (add connection (file what)))

(defmethod-command add ((what string))
  (check-args string what)
  (send "add" what))

(defgeneric add-id (connection what)
  (:documentation "Like add, but returns a id."))

(defmethod-command add-id ((what track))
  (add connection (file what)))

(defmethod-command add-id ((what string))
  (check-args string what)
  (car (filter-keys (send "addid" what))))

(defcommand move (from to)
  "Move track from `from' to `to' in the playlist."
  (check-args unsigned-byte from to)
  (unless (= from to)
    (send "move" from to)))

(defgeneric move-id (connection id to)
  (:documentation "Move track with `id' to `to' in the playlist."))

(defmethod-command move-id ((track playlist) (to integer))
  (move-id connection (id track) to))

(defmethod-command move-id ((id integer) (to integer))
  (check-args unsigned-byte id to)
  (send "moveid" id to))

(defcommand swap (first second)
  "Swap positions of two tracks."
  (check-args unsigned-byte first second)
  (unless (= first second)
    (send "swap" first second)))

(defgeneric swap-id (connection first second)
  (:documentation "Swap positions of two tracks by id."))

(defmethod-command swap-id ((first playlist) (second playlist))
  (swap-id connection (id first) (id second)))

(defmethod-command swap-id ((first integer) (second integer))
  (check-args unsigned-byte first second)
  (send "swap" first second))

(defcommand delete-track (number)
  "Delete track from playlist."
  (check-args unsigned-byte number)
  (send "delete" number))

(defgeneric delete-id (connection id)
  (:documentation "Delete track with `id' from playlist."))

(defmethod-command delete-id ((id playlist))
  (delete-id connection (id id)))

(defmethod-command delete-id ((id integer))
  (check-args unsigned-byte id)
  (send "deleteid" id))

(defcommand shuffle ()
  "Shuffle the current playlist."
  (send "shuffle"))

;;; Database

(defcommand update (&optional path)
  "Scan directory for music files and add them to the database."
  (check-args string path)
  (send "update" path))

(defcommand find-tracks (type what)
  "Find tracks in the database with a case sensitive, exact match."
  (check-args tag-type type)
  (check-args string what)
  (parse-list (send "find" type what) 'track))

(defcommand list-metadata (metadata-1 &optional metadata-2 search-term)
  "List all metadata of `metadata-1'.
If `metadata-2' & `search-term' are supplied,
then list all `metadata-1' in which `metadata-2' has value `search-term'."
  (check-args (or string null) search-term)
  (send "list" metadata-1 metadata-2 search-term))

(defcommand search-tracks (type what)
  "Find tracks in the database with a case sensitive, inexact match."
  (check-args tag-type type)
  (check-args string what)
  (parse-list (send "search" type what) 'track))

(defcommand list-all-info (&optional path)
  "Lists all information about files in `path' recursively. Default path is /."
  (parse-list (send "listallinfo" path) 'track))

(defcommand list-all (&optional path)
  "Lists all files in `path' recursively. Default path is /."
  (check-args (or string null) path)
  (filter-keys (send "listall" path)))

(defcommand list-info (&optional path)
  "Show contents of directory."
  (check-args (or string null) path)
  (parse-list (send "lsinfo" path) 'track))

(defcommand count-tracks (scope query)
  "Number of songs and their total playtime matching `query'.
Return: (number playtime)."
  (check-args string query)
  (filter-keys (send "count" scope query)))

(defcommand tag-types ()
  "Get a list of available metadata types."
  (filter-keys (send "tagtypes")))

(defcommand url-handlers ()
  "Get a list of available URL handlers."
  (filter-keys (send "urlhandlers")))

(defun (setf volume) (value connection)
  "Set the volume to the value between 0-100."
  (check-type value (integer 0 100) "an integer in range 0-100")
  (send "setvol" value))

(defun (setf randomized) (value connection)
  "NIL---turn off random mode, non-nil---turn on random mode."
  (send "random" (if value 1 0)))

(defun (setf repeat) (value connection)
  "NIL---turn off repeat mode, non-nil---turn on repeat mode."
  (send "repeat" (if value 1 0)))

(defcommand seek (song time)
  "Skip to a specified point in a song on the playlist."
  (send "seek" song time))

(defgeneric seek-id (connection song time)
  (:documentation "Skip to a specified point in a song on the playlist."))

(defmethod-command seek-id ((song playlist) (time integer))
  (seek-id connection (id song) time))

(defmethod-command seek-id ((song integer) (time integer))
  (check-args unsigned-byte song time)
  (send "seekid" song time))
