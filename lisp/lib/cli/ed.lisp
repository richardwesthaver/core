;;; lib/cli/ed.lisp --- Editor functions

;;

;;; Code:
(in-package :cli/ed)

(defvar *user-emacs-directory* (merge-pathnames ".emacs.d/" (user-homedir-pathname)))


(defmacro with-emacs-printer (&body body)
  "Eval BODY with Emacs Lisp printer settings."
  `(let ((*print-case* :downcase))
     ,@body))

(defun run-emacs (args &key file create-frame eval client)
  (when client 
    (return-from run-emacs 
      (run-emacsclient args :file file :create-frame create-frame :eval eval)))
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

(defun eval-emacs (form &key (client t) args file)
  (run-emacs args :eval form :file file :client client))

(defun ielm (&optional buf-name)
  (eval-emacs `(ielm ,buf-name)))

(defun slime (&optional command coding-system)
  (eval-emacs `(slime ,command ,coding-system)))

(push #'run-emacsclient sb-ext:*ed-functions*)
(push #'run-emacs sb-ext:*ed-functions*)

(defmacro with-emacs ((var &key (eval t) (client t) create-frame file args) &body body)
  (if (eql t eval)
      `(progn (eval-emacs `(progn ,,@body) :client ,client :args ,args))
      `(let ((,var (run-emacs ,args :eval ,eval :file ,file :create-frame ,create-frame)))
         ,@body)))

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
