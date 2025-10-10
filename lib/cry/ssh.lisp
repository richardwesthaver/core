;;; ssh.lisp --- SSH

;; 

;;; Code:
(in-package :cry/ssh)

(defvar *user-ssh-directory* (merge-homedir-pathnames ".ssh/"))

(definline user-ssh-config-file () (probe-file (merge-pathnames *user-ssh-directory* "config")))
(definline system-ssh-config-file () (probe-file "/etc/ssh/ssh_config"))
(definline system-sshd-config-file () (probe-file "/etc/ssh/sshd_config"))

(defconfig ssh-config (ast) ())
(defconfig sshd-config (ssh-config) ())

(defun load-ssh-config-file (path)
  (with-open-file (f path)
    (map 'list
         (lambda (x)
           (mapcar #'trim (split-sequence #\space (string-trim '(#\tab #\space) x) :count 2)))
         (loop for l = (read-line f nil nil)
               until (null l)
               unless (or (zerop (length l)) (char= (char l 0) #\#))
               collect l))))

(defmethod make-config ((self (eql :ssh)) &key (path (user-ssh-config-file)))
  (let ((cfg (load-ssh-config-file path))
        (host) (match) (opts))
    (loop for c in cfg
          do (string-case ((string-downcase (car c)))
               ("host" (when host (push (nreverse host) opts))
                       (when match (push (nreverse match) opts))
                       (setq host (list (cdr c) :host)
                             match nil))
               ("match" (when match (push (nreverse match) opts))
                        (when host (push (nreverse host) opts))
                        (setq match (list (cdr c) :match)
                              host nil))
               (t
                (cond 
                  (host (push c host))
                  (match (push c match))
                  (t (push c opts)))))
          finally (progn
                    (cond (host (push (nreverse host) opts))
                          (match (push (nreverse match) opts)))
                    (nreversef opts)))
    (make-instance 'ssh-config :ast opts)))

(defclass ssh-socket (tcp-socket) ())
