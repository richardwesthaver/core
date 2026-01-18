;;; cli.lisp --- MPK CLI

;; 

;;; Code:
(in-package :mpk/cli)

(defcommand (:mpk stats) ()
  (when-let* ((*mpc* (ignore-errors (mpd:mpc-connect)))
              (state (mpd::state (mpd:mpc-status *mpc*))))
    (case state
      (:play (describe (mpd:mpc-playing *mpc*))))))

(defcommand (:mpk play) ()
  (if (zerop *argc*)
      (mpk-play nil)
      (if-let ((path (and #1=(car *args*) (probe-file #1#))))
        (mpk-play path)
        (apply 'mpk-play *args*))))

(defcommand (:mpk toggle) ()
  (mpk-toggle :mpd))

(defcommand (:mpk stop) ()
  (mpk-stop :mpd))

(defcommand (:mpk pause) ()
  (mpk-pause :mpd))

(defcommand (:mpk next) ()
  (mpk-next :mpd))

(defcommand (:mpk prev) ()
  (mpk-prev :mpd))

(defcommand (:mpk shuffle) ()
  (mpk-shuffle :mpd))

(defcommand (:mpk get) ())

(defcommand (:mpk mpc) ()
  (with-package :mpk-user
    (mpd:ensure-mpd)
    (mpd:ensure-mpc (*mpc*))
    (cli/linedit:install-repl)
    (sb-impl::toplevel-repl nil)))

(define-command-type (:mpk mpd-stats) (&optional val)
  (when val
    (mpd:with-mpc (*mpc*)
      (let ((*print-slot-indent* 2))
	(print *mpc*)
	(print-slots (mpd:mpc-status *mpc*))
	(print-slots (mpd:mpc-stats *mpc*)))
      (terpri))))

(define-cli "mpk"
  :version "0.1.0"
  :description "Media Production Kit"
  :package :mpk
  :kernel (with-commands :mpk (command 'stats)))

(defmain start-mpk ()
  (with-cli ((cli :mpk))
    (with-package :mpk-user
      (funcall (kernel *cli*)))))
