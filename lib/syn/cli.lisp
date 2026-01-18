;;; cli.lisp --- SYN CLI Tools

;; 

;;; Code:
(in-package :syn/cli)

(defcommand (:syn print) (&rest args)
  (when args
    (let ((f (car args)))
      (if (probe-file f)
	  (print-code (syn/gen/c::read-c-file f))
	  (print-code (syn/gen/c::read-c-string f)))
      (terpri))))

(define-command-type (:syn :syntax) (arg)
  (let ((syn (keywordicate (string-upcase arg))))
    (syn/gen:load-gen syn)
    (setq *package* (syn/gen:gen-package syn))
    syn))

(define-cli "gen"
  :package :syn/gen 
  :version 0
  :kernel (with-commands :syn (command 'print))
  :description "Syntax GENerator")
