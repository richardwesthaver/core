;;; cli.lisp --- MPK CLI

;; 

;;; Code:
(in-package :mpk/cli)

(defcmd mpk-stats-cmd ()
  (mpd:with-mpc (*mpc*)
    (let ((*print-slot-indent* 2))
      (print *mpc*)
      (print-slots (mpd:mpc-status *mpc*))
      (print-slots (mpd:mpc-stats *mpc*)))
    (terpri)))

(defcmd mpk-play-cmd ()
  (let ((arg (car *args*)))
    (if (null arg)
        (mpd:with-mpc (*mpc*) (mpd:mpc-play *mpc*))
        (if-let ((path (probe-file arg)))
          (let ((ext (pathname-type path)))
            (cond 
              ((null ext) (mpk-play path)) ;; directory
              ;; ((member (string-downcase ext) *known-media-types*) (mpk-play file))
              (t #+nil (unknown-file-type file) (mpk-play path))))
          (mpk-play arg)))))

(defcmd mpk-pause-cmd ()
  (mpk-pause :mpd))

(defcmd mpk-mpc-cmd ()
  (with-package :mpk-user
    (mpd:with-mpc (*mpc*)
      (loop (progn
              (format t "~%(*mpc*) >> ")
              (let ((form (read)))
                (print (funcall (sb-cltl2:enclose `(lambda () ,form))))))))))

(define-cli *mpk-cli*
  :name "mpk"
  :help t
  :description "Media Production Kit"
  :cmds ((:name play :thunk mpk-play-cmd)
         (:name pause :thunk mpk-pause-cmd)
         (:name mpc :thunk mpk-mpc-cmd)
         (:name stats :thunk mpk-stats-cmd))
  :thunk mpk-stats-cmd)

(load-package-cli *mpk-cli* :package :mpk)

(defmain start-mpk ()
  (with-cli ((package-cli :mpk) :args (args))
    (with-package :mpk-user
      (cli:do-cmd cli:*cli*))))
