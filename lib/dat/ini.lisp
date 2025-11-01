;;; ini.lisp --- INI Format

;; https://en.wikipedia.org/wiki/INI_file

;;; Code:
(in-package :dat/ini)

(defclass ini-object (ast) ())
(defclass ini-document (ini-object) ())
(defclass ini-section (ini-object) ())
;; (defun ini-write (value &optional stream))

(defaccessor name ((self ini-section)) (car (ast self)))

(defmethod print-object ((self ini-object) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (car (ast self)))))

(defun ini-peek-char (stream expected &key skip-ws)
  (when (equal (peek-char skip-ws stream nil) expected)
    (read-char stream)))

(defun ini-read-char (stream expected &key skip-ws)
  (declare (optimize (speed 3) (debug 0)))
  (if (ini-peek-char stream expected :skip-ws skip-ws)
      t
    (error "INI error: unexpected ~s~%expected ~A" (read-char stream) expected)))

(defun ini-read (stream &optional (eof-error-p t) eof-value)
  (let ((c (peek-char t stream eof-error-p :eof)))
    (case c
      (:eof eof-value)
      (#\[ (ini-read-section stream))
      (#\# (ini-read-comment stream) (ini-read stream eof-error-p eof-value))
      (t (ini-read-pair stream)))))

(defun ini-key-char-p (c)
  (or (digit-char-p c) (sb-unicode:alphabetic-p c) (char= #\- c) (char= #\_ c)))

(defun ini-peek-key-char (stream)
  (let ((c (peek-char t stream nil nil)))
    (when (and c (ini-key-char-p c))
      c)))

(defun ini-read-name (stream)
  (with-output-to-string (s)
    (loop for c = (peek-char nil stream nil nil)
          while (and c (not (char= c #\])))
          do (write-char (read-char stream) s))
    s))

(defun ini-read-key (stream)
  (with-output-to-string (s)
    (loop for c = (peek-char t stream nil nil)
          while (and c (ini-key-char-p c))
          do (write-char (read-char stream) s))
    s))

(defun ini-read-pair (stream)
  (let ((line (split-sequence #\= (read-line stream) :count 2)))
    (unless (sequence:emptyp (car line))
      (let ((l (mapcar 'trim line)))
        (cons (car l) (cadr l))))))

(defun ini-read-section (stream)
  (ini-read-char stream #\[ :skip-ws t)
  (let ((ret (list (ini-read-name stream))))
    (ini-read-char stream #\] :skip-ws t)
    (loop while (or (ini-read-comment stream) (ini-peek-key-char stream))
          do (push (ini-read-pair stream) ret))
    (make-instance 'ini-section
      :ast (nreverse ret))))

(defun ini-read-comment (stream)
  (loop while (ini-peek-char stream #\# :skip-ws t)
        do (read-line stream)))

(defun ini-read-document (stream)
  (make-instance 'ini-document
    :ast
    (loop for x = (ini-read stream nil nil)
          while x
          collect x)))

;;; Serde
(defmethod deserialize ((from stream) (format (eql :ini)) &key)
  (ini-read-document from))

(defmethod deserialize ((from string) (format (eql :ini)) &key)
  (with-input-from-string (s from)
    (ini-read-document s)))

(defmethod deserialize ((from pathname) (format (eql :ini)) &key)
  (with-open-file (f from)
    (ini-read-document f)))

;;; Desktop Entry
(defclass desktop-entry (ini-document)
  ((name :accessor name)
   (type)
   (exec :accessor exec)
   (categories :initform nil)
   (no-display :type boolean)
   generic-name
   comment
   startup-wm-class
   keywords
   mime-type
   try-exec
   icon
   (terminal :type boolean)))

(defmethod print-object ((self desktop-entry) stream)
  (format stream "(desktop-entry :name ~S :categories ~S :no-display ~S)"
          (name self) (slot-value self 'categories) (slot-value self 'no-display)))

(defmethod load-ast ((self desktop-entry))
  (when-let ((props (cdr (ast (car (ast self))))))
    (flet ((dget (n) (cdr (assoc n props :key 'string-downcase :test 'equal)))
           (bool (x) (when (equal (trim (string-downcase x)) "true") t)))
      (setf (name self) (dget "name")
            (slot-value self 'type) (dget "type")
            (exec self) (dget "exec")
            (slot-value self 'terminal) (bool (dget "terminal"))
            (slot-value self 'categories) (when-let ((cats (dget "categories"))) (ssplit #\; cats))
            (slot-value self 'no-display) (bool (dget "nodisplay"))
            (slot-value self 'generic-name) (dget "genericname")
            (slot-value self 'comment) (dget "comment")
            (slot-value self 'startup-wm-class) (dget "startupwmclass")
            (slot-value self 'keywords) (when-let ((kws (dget "keywords"))) (ssplit #\; kws))
            (slot-value self 'mime-type) (when-let ((mts (dget "mimetype"))) (ssplit #\; mts))
            (slot-value self 'try-exec) (dget "tryexec")
            (slot-value self 'icon) (dget "icon"))
    self)))

(defmethod deserialize ((from t) (format (eql :desktop-entry)) &key)
  (load-ast (change-class (deserialize from :ini) 'desktop-entry)))

(defmethod equiv:equiv ((a desktop-entry) (b desktop-entry))
  (and (string= (name a) (name b))
       (string= (slot-value a 'type) (slot-value b 'type))
       (string= (slot-value a 'exec) (slot-value b 'exec))))

(defmethod equiv:eqv ((a desktop-entry) (b desktop-entry))
  (and (string= (name a) (name b))
       (string= (slot-value a 'type) (slot-value b 'type))
       (string= (slot-value a 'exec) (slot-value b 'exec))
       (equalp (slot-value a 'categories) (slot-value b 'categories))
       (equalp (slot-value a 'no-display) (slot-value b 'no-display))
       (equalp (slot-value a 'only-show-in) (slot-value b 'only-show-in))
       (equalp (slot-value a 'terminal) (slot-value b 'terminal))))

(defun desktop-entry-in-categories-p (entry seq)
  (every #'(lambda (c)
             (some #'(lambda (e) (string= c e))
                   (slot-value entry 'categories)))
         seq))

(defvar *desktop-entry-main-categories*
  (list
   "AudioVideo"
   "Audio"
   "Video"
   "Development"
   "Education"
   "Game"
   "Graphics"
   "Network"
   "Office"
   "Settings"
   "System"
   "Utility"))
(defvar *desktop-entry-favorite-category* "Favorite")
(defvar *desktop-entry-paths*
  '(#P"/usr/share/applications"
    #P"~/.local/share/applications"))
