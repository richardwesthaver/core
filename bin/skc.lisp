;;; skc.lisp --- Skel Client

;; 

;;; Code:
(in-package :std-user)
(defpkg :bin/skc
  (:use :cl :std :cli :skel/net/client :clap)
  (:nicknames :skc))

(in-package :bin/skc)

(defcmd skc ()
  (log:info! :args *args* :opts *opts*))

(define-cli *skc-cli*
  :name "skc"
  :help t
  :version "0.1.0"
  :description "Skel Client"
  :version #.(format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :thunk skc)

(defmain start-skc ()
  (with-cli (*skc-cli* :run t :exit t) (describe *cli*)))
