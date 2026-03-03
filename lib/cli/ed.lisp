;;; lib/cli/ed.lisp --- Editor functions

;;

;;; Code:
(in-package :cli/ed)

(init :commands :name :ed :class 'editor-command :clean t)

(defvar *editor* nil)

(defvar *user-emacs-directory* (std:xdg-config-dir :emacs))
(defvar *user-org-directory* (merge-homedir-pathnames "org/"))

(defmacro with-emacs-printer (&body body)
  "Eval BODY with Emacs Lisp printer settings."
  `(let ((*print-case* :downcase)
         (*print-readably* nil))
     ,@body))

(defun run-emacsclient (args &key file (create-frame t) function eval wait output server input)
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

(push #'run-emacsclient sb-ext:*ed-functions*)
(push #'run-emacs sb-ext:*ed-functions*)

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

(defmethod make-config ((fmt (eql :emacs)) &key ast path default user)
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

;;; Commands
(defkernel editor-command (command) ()
  (:documentation "Class of COMMANDs which are executed by an EDITOR."))


;; TODO 2025-09-19: 
;; (defun edit-line (file &key line start end)
;;   "A simple lisp line editor.")
