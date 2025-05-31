;;; cli.lisp --- MPK CLI

;; 

;;; Code:
(in-package :mpk/cli)

(defcmd mpk-stats-cmd ()
  (when-let* ((*mpc* (ignore-errors (mpd:mpc-connect)))
              (state (mpd::state (mpd:mpc-status *mpc*))))
    (case state
      (:play (describe (mpd:mpc-playing *mpc*))))))

(defcmd mpk-play-cmd ()
  (if (zerop *argc*)
      (mpk-play nil)
      (if-let ((path (and #1=(car *args*) (probe-file #1#))))
        (mpk-play path)
        (apply 'mpk-play *args*))))

(defcmd mpk-toggle-cmd ()
  (mpk-toggle :mpd))

(defcmd mpk-stop-cmd ()
  (mpk-stop :mpd))

(defcmd mpk-pause-cmd ()
  (mpk-pause :mpd))

(defcmd mpk-next-cmd ()
  (mpk-next :mpd))

(defcmd mpk-prev-cmd ()
  (mpk-prev :mpd))

(defcmd mpk-shuffle-cmd ()
  (mpk-shuffle :mpd))

(defcmd mpk-get-cmd ())

(defcmd mpk-mpc-cmd ()
  (with-package :mpk-user
    (mpd:ensure-mpd)
    (mpd:ensure-mpc (*mpc*))
    (cli/linedit:install-repl)
    (sb-impl::toplevel-repl nil)))

(defopt mpd-stats-opt
  (when *arg*
    (mpd:with-mpc (*mpc*)
      (let ((*print-slot-indent* 2))
	(print *mpc*)
	(print-slots (mpd:mpc-status *mpc*))
	(print-slots (mpd:mpc-stats *mpc*)))
      (terpri))))

(defcmd mpk-daemon* ()
  (with-cli (*mpkd-cli* :args (cdr (cli:args)))
    (do-opts *mpkd-cli*)
    (do-cmd *mpkd-cli*)))

(define-cli *mpkd-cli*
  :name "mpkd"
  :version "0.1.0"
  :help t
  :description "Media Production Kit Daemon")

(define-cli *mpk-cli*
  :name "mpk"
  :version "0.1.0"
  :help t
  :description "Media Production Kit"
  :opts #1=((:name "mpd" :description "include MPD stats" :kind boolean :thunk mpd-stats-opt))
  :cmds ((:name play :thunk mpk-play-cmd)
	 (:name toggle :thunk mpk-toggle-cmd)
	 (:name pause :thunk mpk-pause-cmd)
	 (:name stop :thunk mpk-stop-cmd)
	 (:name next :thunk mpk-next-cmd)
	 (:name prev :thunk mpk-prev-cmd)
	 (:name shuffle :thunk mpk-shuffle-cmd)
	 (:name mpc :thunk mpk-mpc-cmd)
         (:name get :thunk mpk-get-cmd)
         (:name daemon :thunk mpk-daemon*)
         (:name stats :thunk mpk-stats-cmd
          :opts #1#))
  :thunk mpk-stats-cmd)

(load-package-cli *mpk-cli* :package :mpk)

(defmain start-mpk ()
  (with-cli ((package-cli :mpk) :args (args))
    (with-package :mpk-user
      (cli:do-cmd cli:*cli*))))
