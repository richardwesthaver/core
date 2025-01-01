;;; lib/cli/ed.lisp --- Editor functions

;;

;;; Code:
(in-package :cli/ed)

(defvar *user-emacs-directory* (merge-pathnames ".emacs.d/" (user-homedir-pathname)))


(defmacro with-emacs-printer (&body body)
  "Eval BODY with Emacs Lisp printer settings."
  `(let ((*print-case* :downcase))
     ,@body))

(defun run-emacs (args &key file create-frame eval)
  (when create-frame (push "-c" args))
  (when file (push (namestring file) args))
  (when eval 
    (with-emacs-printer
      (appendf args (list "-e" (format nil "~A" eval)))))
  (sb-ext:run-program (find-exe "emacs") args))
  
(defun run-emacsclient (args &key file (create-frame t) (eval))
  (when create-frame (push "-c" args))
  (when file (push (namestring file) args))
  (push "-a=" args)
  (when eval 
    (with-emacs-printer
      (appendf args (list "-e" (format nil "~A" eval)))))
  (sb-ext:run-program (find-exe "emacsclient")
                      args
                      :wait nil
                      :output nil))

(defun eval-emacs (form &key (client t) args)
  (if client
      (run-emacsclient args :eval form)
      (run-emacs args :eval form)))

(defun ielm (&optional buf-name)
  (eval-emacs `(ielm ,buf-name)))

(defun slime (&optional command coding-system)
  (eval-emacs `(slime ,command ,coding-system)))

(push #'run-emacsclient sb-ext:*ed-functions*)
(push #'run-emacs sb-ext:*ed-functions*)

;;; Config
(defconfig editor-config (ast) ())

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

;;; Macros
