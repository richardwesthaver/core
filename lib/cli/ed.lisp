;;; lib/cli/ed.lisp --- Editor functions

;;

;;; Code:
(in-package :cli/ed)

(defvar *user-emacs-directory* (merge-pathnames ".emacs.d/" (user-homedir-pathname)))

(defmacro with-emacs-printer (&body body)
  "Eval BODY with Emacs Lisp printer settings."
  `(let ((*print-case* :downcase)
         (*print-readably* nil))
     ,@body))

(defun run-emacs (args &key file create-frame eval client wait)
  (if client
      (run-emacsclient args :file file :create-frame create-frame :eval eval :wait wait)
      (let ((keys))
        (when file (push (format nil "~S" file) keys))
        (when create-frame (push "-c" keys))
        (when eval 
          (with-emacs-printer
            (appendf keys (list "-e" (format nil "~S" eval)))))
        (sb-ext:run-program (find-exe "emacs") (print (append (nreverse keys) args))))))

(defun run-emacsclient (args &key file (create-frame t) eval wait)
  (let ((keys))
    (when file (push (format nil "~S" file) keys))
    (when create-frame (push "-c" keys))
    (push "-a=" keys)
    (when eval
      (with-emacs-printer
        (appendf keys (list "-e" (format nil "~S" eval)))))
    (sb-ext:run-program (find-exe "emacsclient")
                        (append (nreverse keys) args)
                        :wait wait
                        :output nil)))

(defun eval-emacs (form &key (client t) args file wait create-frame)
  (run-emacs args :eval form :file file :client client :wait wait :create-frame create-frame))

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

;;; Config
(defconfig editor-config (ast) ())

(defmethod make-config ((fmt (eql :editor)) &rest initargs &key type &allow-other-keys)
  (if type
      (progn
        (remf initargs :type)
        (apply 'make-config type initargs))
      (make-instance 'editor-config)))

(defconfig emacs-config (editor-config) 
  ((path :initform *user-emacs-directory* :initarg :path :accessor path)))

(defun load-emacs-config (&optional (path *user-emacs-directory*))
  (make-config :emacs :path path))

(defmethod make-config ((fmt (eql :emacs)) &key ast path)
  (make-instance 'emacs-config :ast ast :path path))

;;; Org Protocol
;; ref: https://orgmode.org/worg/org-contrib/org-protocol.html

;; On GNU/Linux, Emacs is now the default application for
;; 'org-protocol'. (startup change in Emacs 30.1)
(defun org-store-link (url title)
  (run-emacsclient (format nil "org-protocol://store-link?url=~a&title=~a"
                           url title)))

(defun emacs-find-file (path &key (position 0) (wait t) create-frame (client t))
  (eval-emacs `(progn (find-file ,path) (goto-char ,position)) :wait wait :create-frame create-frame :client client))

;;; Macros
(defmacro with-emacs ((var &key (eval t) (client t) create-frame file (wait t) args) &body body)
  (if (eql t eval)
      `(progn (eval-emacs '(progn ,@body) :client ,client :args ,args :wait ,wait))
      `(let ((,var (run-emacs ,args :eval ,eval :file ,file :create-frame ,create-frame :wait ,wait)))
         ,@body)))
