;;; cli.lisp --- MPK CLI

;; 

;;; Code:
(in-package :mpk/cli)

(defcmd mpk-play-cmd ()
  (if-let ((file (probe-file (car *args*))))
    (let ((ext (pathname-type file)))
      (cond 
        ((null ext) (mpk-play file)) ;; directory
        ;; ((member (string-downcase ext) *known-media-types*) (mpk-play file))
        (t #+nil (unknown-file-type file) (mpk-play file))))
      (error 'sb-ext:file-does-not-exist :pathname (car *args*))))

(defcmd mpk-mpc-cmd ()
  (mpd:with-mpc (c)
    (format t "~%with-mpc (c) >> ")
    (funcall (sb-cltl2:enclose `(lambda (&optional (c ,c)) ,(read))))))

(define-cli *mpk-cli*
  :name "mpk"
  :help t
  :description "Media Production Kit"
  :cmds ((:name play :thunk mpk-play-cmd)
         (:name mpc :thunk mpk-mpc-cmd)))
