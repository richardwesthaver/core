;;; lib/cli/ed.lisp --- Editor functions

;;

;;; Code:
(in-package :cli/ed)

(init :commands :name :ed :class 'editor-command :clean t :names t)

(defvar *editor* nil)
(defvar *editor-config*)

;;; Emacs
(defvar *user-emacs-directory* (std:xdg-config-dir :emacs))
(defvar *user-org-directory* (merge-homedir-pathnames "org/"))

(defmacro with-emacs-printer (&body body)
  "Eval BODY with Emacs Lisp printer settings."
  `(let ((*print-case* :downcase)
         (*print-readably* nil))
     ,@body))

(defun run-emacsclient (args &key file create-frame function eval wait output server input)
  (let ((keys))
    (when file (push (format nil "~S" file) keys))
    (when create-frame (push "-c" keys))
    (when function (appendf keys (list "-f" (string-downcase function))))
    (when server (appendf keys (list "-s" (string-downcase server))))
    (push "-a=" keys)
    (when eval
      (with-emacs-printer
        (appendf keys (list "-e" (format nil "~S" eval)))))
    (sb-ext:run-program (find-exe "emacsclient")
                        (append keys args)
                        :wait wait
                        :output output
                        :input input)))

(defun run-emacs (args &key file create-frame eval client wait batch function output input server)
  (if client
      (run-emacsclient args :file file 
                            :create-frame create-frame 
                            :eval eval :wait wait :output output :server server
                            :input input)
      (let ((keys))
        (when file (push (format nil "~S" file) keys))
        (when create-frame (push "-c" keys))
        (when function (appendf keys (list "-f" (string-downcase function))))
        (when batch (push "--batch" keys))
        (when server (push
                      (if (eql t server) 
                          "--daemon" ; background daemon
                          (format nil "--fg-daemon=~(~A~)" server))
                      keys))
        (when eval 
          (with-emacs-printer
            (appendf keys (list "-e" (format nil "~S" eval)))))
        (sb-ext:run-program (find-exe "emacs") (append keys args) :wait wait :output output :input input))))

(defun eval-emacs (form &key (client t) args file wait create-frame batch function output server input)
  (run-emacs args :eval form 
                  :file file 
                  :client client 
                  :wait wait 
                  :create-frame create-frame 
                  :batch batch
                  :function function
                  :output output
                  :input input
                  :server server))

(defun ielm (&optional buf-name)
  (eval-emacs `(ielm ,@(when buf-name `(,buf-name)))))

(defun slime (&optional command coding-system)
  (eval-emacs `(slime ,command ,coding-system)))

(defun ediff (a b)
  (eval-emacs `(ediff ,(namestring a) ,(namestring b))))

(defun ediff3 (a b c)
  (eval-emacs `(ediff ,(namestring a) ,(namestring b) ,(namestring c))))

(defun vc-ediff (&optional rev-a rev-b)
  "Show differences between REV1 and REV2 of FILES using ediff.
This compares two revisions of the files in FILES.  Currently,
only a single file's revisions can be compared, i.e. FILES can
specify only one file name.
If REV1 is nil, it defaults to the current revision, i.e. revision
of the last commit.
If REV2 is nil, it defaults to the work tree, i.e. the current
state of each file in FILES."
  (eval-emacs
   (if (or rev-a rev-b)
       `(vc-version-ediff nil ,rev-a ,rev-b)
       `(vc-ediff t))
   :wait t
   :create-frame t))

;;; Conditions
;; TODO 2025-11-01: 'EDIT' restart

;;; Config
(defconfig editor-config (ast) ())

(defmethod make-config ((fmt (eql :editor)) &rest initargs &key type &allow-other-keys)
  (if type
      (progn
        (remf initargs :type)
        (apply 'make-config type initargs))
      (make-instance 'editor-config)))

(defconfig emacs-config (editor-config)
  ((path :initform *user-emacs-directory* :initarg :path :accessor path)
   default user))

(defun load-emacs-config (&optional (path *user-emacs-directory*))
  (make-config :emacs :path path 
                      :default (merge-pathnames "default.el" path)
                      :user (merge-pathnames (format nil "~(~A~).el" (sb-posix:getenv "USER")) path)))

(defmethod make-config ((fmt (eql :emacs)) 
                        &key ast 
                             (path *user-emacs-directory*) 
                             (default (merge-pathnames "default.el" path))
                             (user (merge-pathnames (format nil "~(~A~).el" (sb-posix:getenv "USER")) path)))
  (make-instance 'emacs-config 
    :ast ast 
    :path path 
    :default (when default (probe-file default))
    :user (when user (probe-file user))))

;;; Org Protocol
;; ref: https://orgmode.org/worg/org-contrib/org-protocol.html

;; On GNU/Linux, Emacs is now the default application for
;; 'org-protocol'. (startup change in Emacs 30.1)
(defun org-store-link (url title)
  (run-emacsclient (format nil "org-protocol://store-link?url=~a&title=~a"
                           url title)))

(defun emacs-find-file (path &key (position 0) (wait t) create-frame (client t))
  (eval-emacs `(progn (find-file ,path) (goto-char ,position)) :wait wait :create-frame create-frame :client client))

(defmacro with-emacs ((var &key (eval t) (client t) create-frame file (wait t) batch function args output input server) &body body)
  (if (eql t eval)
      `(progn (eval-emacs '(progn ,@body) :client ,client :args ,args :wait ,wait :batch ,batch :function ,function :output ,output :server ,server :input ,input :create-frame ,create-frame))
      `(let ((,var (run-emacs ,args :eval ,eval 
                                    :file ,file 
                                    :create-frame ,create-frame 
                                    :wait ,wait 
                                    :batch ,batch
                                    :function ,function
                                    :output ,output
                                    :input ,input
                                    :server ,server)))
         ,@body)))

;;; Mixin that implements undo
(eval-always
  (defclass rewindable ()
    ((data :reader data
           :initform (make-array 12 :fill-pointer 0 :adjustable t))
     ;; Index is the number of rewinds we've done.
     (idx :accessor idx
          :initform 0)))

  (defun %rewind-count (rewindable)
    (fill-pointer (data rewindable)))

  (defun last-state (rewindable)
    (let ((size (%rewind-count rewindable)))
      (if (zerop size)
          (values nil nil)
          (values (aref (data rewindable) (1- size)) t))))

  (defun save-rewindable-state (rewindable object)
    (let ((index (idx rewindable))
          (store (data rewindable)))
      (unless (zerop index)
        ;; Reverse the tail of pool, since we've
        ;; gotten to the middle by rewinding.
        (setf (subseq store index) (nreverse (subseq store index))))
      (vector-push-extend object store)))

  (defmethod rewind-state ((rewindable rewindable))
    (assert (not (zerop (%rewind-count rewindable))))
    (setf (idx rewindable) 
          (mod (1+ (idx rewindable)) (%rewind-count rewindable)))
    (aref (data rewindable) 
          (- (%rewind-count rewindable) (idx rewindable) 1))))

(defclass line ()
  ((string :accessor get-string :initform "" :initarg :string)
   (point :accessor get-point :initform 0 :initarg :point)))

(defmethod (setf get-string) :around (string line)
  (prog1 (call-next-method)
    (when (>= (get-point line) (length string))
      (setf (get-point line) (length string)))))

(defmethod (setf get-point) :around (point line)
  (when (<= 0 point (length (get-string line)))
    (call-next-method)))

;;; Text Buffer
;; BUFFER offers a simple browsable from of storage. It is used to
;; implement both the kill-ring and history.
(defclass text-buffer ()
  ((prev :initarg :prev :accessor prev :initform nil)
   (next :initarg :next :accessor next :initform nil)
   (data :initarg :data :accessor data :initform nil)
   ;; For file-backed buffers.
   (path :initarg :path :initform nil :accessor path)))

(defun copy-buffer (buffer)
  (make-instance 'text-buffer
    :prev (prev buffer)
    :next (next buffer)
    :data (data buffer)
    :path (path buffer)))

(defun ensure-buffer (datum)
  "DATUM may be a buffer, a list, or a pathname designator."
  (etypecase datum
    (text-buffer datum)
    ((or pathname string null)
     (let ((buffer (make-instance 'text-buffer :path datum)))
       (when datum
         (with-open-file (f datum
                            :direction :input
                            :if-does-not-exist nil
                            :external-format :utf-8)
           (when f
             (loop for line = (read-line f nil)
                   while line
                   do (push line (data buffer)))
             (setf (prev buffer) (data buffer)))))
       buffer))
    (list (let ((buffer (make-instance 'text-buffer :data datum)))
            (setf (prev buffer) (data buffer))
            buffer))))

(defun buffer-push (string buffer)
  (unless (equal string (car (data buffer)))
    (push string (data buffer))
    (let ((pathname (path buffer)))
      (when pathname
        (with-open-file (f pathname
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :append
                           :external-format :utf-8)
          (write-line string f))))
    (setf (next buffer) nil
          (prev buffer) (data buffer))))

(defun buffer-find-previous-if (test buffer)
  (std:awhen (position-if test (prev buffer))
    (loop repeat (1+ std:it)
          do (push (pop (prev buffer))
                   (next buffer)))
    (car (next buffer))))

(defun buffer-previous (string buffer)
  (when (prev buffer)
    (push string (next buffer))
    (pop (prev buffer))))

(defun buffer-peek (buffer)
  (std:aif (prev buffer)
           (car std:it)))

(defun buffer-find-next-if (test buffer)
  (std:awhen (position-if test (next buffer))
    (loop repeat (1+ std:it)
          do (push (pop (next buffer)) (prev buffer)))
    (car (prev buffer))))

(defun buffer-next (string buffer)
  (when (next buffer)
    (push string (prev buffer))
    (pop (next buffer))))

(defun buffer-cycle (buffer)
  (flet ((wrap-buffer ()
           (unless (prev buffer)
             (setf (prev buffer) (reverse (next buffer))
                   (next buffer) nil))))
    (wrap-buffer)
    (push (pop (prev buffer)) (next buffer))
    (wrap-buffer)
    t))

;;; Editor
(defclass editor (line rewindable) ())

(defgeneric editor-insert-mode (self)
  (:method ((self editor)) t))
(defgeneric (setf editor-insert-mode) (new self))

(defun save-state (editor)
  (let ((string (get-string editor))
        (last (last-state editor)))
    (unless (and last (equal string (get-string last)))
      ;; Save only if different than last saved state
      (save-rewindable-state editor
                             (make-instance 'line
                               :string (copy-seq string)
                               :point (get-point editor))))))

(defmethod rewind-state ((editor editor))
  (let ((line (call-next-method)))
    (setf (get-string editor) (copy-seq (get-string line))
          (get-point editor) (get-point line))))

(eval-always
  (defmacro with-editor-point-and-string (((point string) editor) &body forms)
    `(let ((,point (get-point ,editor))
           (,string (get-string ,editor)))
       ,@forms)))

;;;; QUOTES
;; (defun dwim-match-quotes (string index))
;; (defun dwim-mark-quotes (string index &key pre post))
;; FIXME: should checking for #\", "\"", et cetera.
(defun quoted-p (string index)
  (let ((quoted-p nil))
    (dotimes (n (min index (length string)) quoted-p)
      (when (eql (schar string n) #\")
        (setf quoted-p (not quoted-p))))))

(defun find-open-quote (string index)
  (when (quoted-p string index)
    (loop for n from (1- index) downto 0
          when (eql (schar string n) #\") return n)))

(defun find-close-quote (string index)
  (when (quoted-p string index)
    (loop for n from (1+ index) below (length string)
          when (eql (schar string n) #\") return n)))

;;;; PARENS
;; FIXME: This is not the Right Way to do paren matching.
;; * use stack, not counting
;; * don't count #\( #\) &co
(defun after-close-p (string index)
  (and (array-in-bounds-p string (1- index))
       (find (schar string (1- index)) ")]}")))

(defun at-open-p (string index)
  (and (array-in-bounds-p string index)
       (find (schar string index) "([{")))

(defun paren-count-delta (char)
  (case char
    ((#\( #\[ #\{) -1)
    ((#\) #\] #\}) 1)
    (t 0)))

(defun find-open-paren (string index)
  (loop with count = 1
        for n from (1- index) downto 0
        do (incf count (paren-count-delta (schar string n)))
        when (zerop count) return n))

(defun find-close-paren (string index)
  (loop with count = -1
        for n from (1+ index) below (length string)
        do (incf count (paren-count-delta (schar string n)))
        when (zerop count) return n))

(defun dwim-match-parens (string index)
  (cond ((after-close-p string index)
         (values (find-open-paren string (1- index)) (1- index)))
        ((at-open-p string index)
         (values index (find-close-paren string index)))
        (t 
         (values nil nil))))

(defun dwim-mark-parens (string index &key pre post)
  (multiple-value-bind (open close) (dwim-match-parens string index)
    (values 
     (if (and open close)
         (concatenate 'simple-string
                      (subseq string 0 open)
                      pre
                      (string (schar string open))
                      post
                      (subseq string (1+ open) close)
                      pre
                      (string (schar string close))
                      post
                      (if (> (length string) (1+ close))
                          (subseq string (1+ close))
                          ""))
         string)
     open)))

(defun editor-word-start (editor)
  "Returns the index of the first letter of current or previous word,
if the point is just after a word, or the point."
  (with-editor-point-and-string ((point string) editor)
    (if (or (not (at-delimiter-p string point))
            (not (and (plusp point) (at-delimiter-p string (1- point)))))
        (1+ (or (position-if 'word-delimiter-p string :end point :from-end t)
                -1)) ; start of string
        point)))

(defun editor-previous-word-start (editor)
  "Returns the index of the first letter of current or previous word,
if the point was at the start of a word or between words."
  (with-editor-point-and-string ((point string) editor)
    (let ((tmp (cond ((at-delimiter-p string point)
                      (position-if-not 'word-delimiter-p string
                                       :end point :from-end t))
                     ((and (plusp point) (at-delimiter-p string (1- point)))
                      (position-if-not 'word-delimiter-p string
                                       :end (1- point) :from-end t))
                     (t point))))
      ;; tmp is always in the word whose start we want (or NIL)
      (1+ (or (position-if 'word-delimiter-p string
                           :end (or tmp 0) :from-end t)
              -1)))))

(defun editor-word-end (editor)
  "Returns the index just beyond the current word or the point if
point is not inside a word."
  (with-editor-point-and-string ((point string) editor)
    (if (at-delimiter-p string point)
        point
        (or (position-if 'word-delimiter-p string :start point)
            (length string)))))

(defun editor-next-word-end (editor)
  "Returns the index just beyond the last letter of current or next
word, if the point was between words."
  (with-editor-point-and-string ((point string) editor)
    (let ((tmp (if (at-delimiter-p string point)
                   (or (position-if-not 'word-delimiter-p string
                                        :start point)
                       (length string))
                   point)))
      ;; tmp is always in the word whose end we want (or already at the end)
      (or (position-if 'word-delimiter-p string :start tmp)
          (length string)))))

(defun editor-word (editor)
  "Returns the current word the point is in or right after, or an
empty string."
  (let ((start (editor-word-start editor))
        (end (editor-word-end editor)))
    (subseq (get-string editor) start end)))

(defun editor-sexp-start (editor)
  (with-editor-point-and-string ((point string) editor)
    (setf point (loop for n from (min point (1- (length string))) downto 0
                      when (not (whitespace-p (schar string n)))
                      return n))
    (case (and point (schar string point))
      ((#\) #\] #\}) (or (find-open-paren string point) 0))
      ((#\( #\[ #\{) (max (1- point) 0))
      (#\" (or (find-open-quote string point)
               (max (1- point) 0)))
      (t (editor-previous-word-start editor)))))

(defun editor-sexp-end (editor)
  (with-editor-point-and-string ((point string) editor)
    (setf point (loop for n from point below (length string)
                      when (not (whitespace-p (schar string n)))
                      return n))
    (case (and point (schar string point))
      ((#\( #\[ #\{) (or (find-close-paren string point)
                         (length string)))
      ((#\) #\] #\}) (min (1+ point) (length string)))
      (#\" (or (find-close-quote string (1+ point))
               (min (1+ point) (length string))))
      (t (editor-next-word-end editor)))))

(defun editor-replace-word (editor word)
  (with-editor-point-and-string ((point string) editor)
    (declare (ignore point))
    (let ((start (editor-word-start editor))
          (end (editor-word-end editor)))
      (setf (get-string editor)
            (concatenate 'simple-string (subseq string 0 start) word (subseq string end))
            (get-point editor) (+ start (length word))))))

(defun in-quoted-string-p (editor)
  (quoted-p (get-string editor) (get-point editor)))

;;; Commands
(defkernel editor-command (command) 
  ((editor :initform *editor* :initarg :editor :accessor editor))
  (:documentation "Class of COMMANDs which use an EDITOR stored in a slot of the same
name (usually same as *EDITOR*."))

;;; BASIC EDITING
(defcommand delete-char-backwards (editor)
  (with-editor-point-and-string ((point string) editor)
    ;; Can't delegate to editor because of the SUBSEQ index calc.
    (unless (zerop point)
      (setf (get-string editor) (concatenate 'simple-string (subseq string 0 (1- point))
                                             (subseq string point))
            (get-point editor) (1- point)))))

(defcommand delete-char-forwards (editor)
  (with-editor-point-and-string ((point string) editor)
    (setf (get-string editor) (concatenate 'simple-string (subseq string 0 point)
                                           (subseq string (min (1+ point) (length string)))))))

(defcommand add-char (editor char)
  (with-editor-point-and-string ((point string) editor)
    (setf (get-string editor)
          (concatenate 'simple-string (subseq string 0 point)
                       (string char)
                       (if (editor-insert-mode editor)
                           (subseq string point)
                           (when (> (length string) (1+ point))
                             (subseq string (1+ point))))))
    (incf (get-point editor))))

(defcommand delete-char-forwards-or-eof (editor)
  (if (equal "" (get-string editor))
      (error 'end-of-file :stream *standard-input*)
      (delete-char-forwards editor)))

(defcommand delete-word-forwards (editor)
  (with-editor-point-and-string ((point string) editor)
    (declare (ignore point))
    (let ((i (get-point editor))
          (j (editor-next-word-end editor)))
      (setf (get-string editor)
            (concatenate 'simple-string (subseq string 0 i) (subseq string j))))))

(defcommand delete-word-backwards (editor)
  (with-editor-point-and-string ((point string) editor)
    (let ((i (editor-previous-word-start editor)))
      (setf (get-string editor) (concatenate 'simple-string (subseq string 0 i)
                                             (subseq string point))
            (get-point editor) i))))

;;; CASE CHANGES
(flet ((frob-case (frob editor)
         (with-editor-point-and-string ((point string) editor)
           (let ((end (editor-next-word-end editor)))
             (setf (get-string editor) (concatenate 'simple-string
                                                    (subseq string 0 point)
                                                    (funcall frob
                                                             (subseq string point end))
                                                    (subseq string end))
                   (get-point editor) end)))))

  (defcommand upcase-word (editor)
    (funcall #'frob-case #'string-upcase editor))

  (defcommand downcase-word (editor)
    (funcall #'frob-case #'string-downcase editor)))

;;; MOVEMENT
(defcommand move-to-bol (editor)
  (setf (get-point editor) 0))

(defcommand move-to-eol (editor)
  (setf (get-point editor) (length (get-string editor))))

(defcommand move-char-right (editor)
  (incf (get-point editor)))

(defcommand move-char-left (editor)
  (decf (get-point editor)))

(defcommand move-word-backwards (editor)
  (setf (get-point editor) (editor-previous-word-start editor)))

(defcommand move-word-forwards (editor)
  (setf (get-point editor) (editor-next-word-end editor)))

(defcommand close-all-sexp (editor)
  (funcall (command "move-to-eol" (commands :ed)) editor)
  (do ((string (get-string editor) (get-string editor)))
      ((not (find-open-paren string (length string))))
    (funcall (command "add-char" (commands :ed)) 
             editor 
             (case (schar string (find-open-paren string (length string)))
               (#\( #\))
               (#\[ #\])
               (#\{ #\})))))

;;; SEXP MOTION
(defcommand forward-sexp (editor)
  (setf (get-point editor) (editor-sexp-end editor)))

(defcommand backward-sexp (editor)
  (setf (get-point editor) (editor-sexp-start editor)))

;;; Editor Functions
;; TODO 2025-09-19: 
;; (defun edit-line (file &key line start end)
;;   "A simple lisp line editor.")
(defun edit-file (file) (run-emacsclient (list (namestring file))))

;;; Prologue
(pushnew #'run-emacs sb-ext:*ed-functions*)
(pushnew #'run-emacsclient sb-ext:*ed-functions*)
(save :commands :ed)
(setq *command-class* 'clap:cli-command)
