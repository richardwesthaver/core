;;; sk.lisp --- Skel Client

;; 

;;; Code:
(in-package :std-user)
(defpkg :bin/sk
  (:use :cl :std :cli :skel/net/client :clap)
  (:nicknames :sk))

(in-package :bin/sk)

(defcmd sk-thunk () (log:info! :args *args* :opts *opts*))

(define-cli *sk-cli*
  :name "sk"
  :help t
  :description "Skel Client"
  :version #.(format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :thunk sk-thunk)

(defmain start-sk ()
  (with-cli (*sk-cli* :run t :exit t) (describe *cli*)))

(start-sk)
