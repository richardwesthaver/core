;;; vc.lisp --- Generic VC Tool

;; 

;;; Code:
(in-package :std-user)
(defpkg :bin/vc
  (:use :cl :std :cli
   :vc :sb-ext :log :cli/clap/util
   :obj/ast)
  (:use :cli/tools/sbcl :cli/prompt)
  (:export :start-vc))

(in-package :bin/vc)

(load-package-cli 
 :vc :opts ((:name "version" :kind boolean :thunk cli::version-opt)))

(defmain start-vc ()
  (in-package :vc)
  (with-cli ((package-cli :bin/vc) :args (cli:args))
    (do-cmd *cli*)))
