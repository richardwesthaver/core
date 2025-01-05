;;; repl.lisp --- CLIM REPL

;; Based on CLIM-LISTENER

;;; Code:
(in-package :gui/clim/repl)

(defun run-clim-listener (&rest args 
                          &key new-process debugger 
                               width height port frame-manager 
                               process-name (package :std-user))
(run-listener :new-process new-process :debugger debugger :width width :height height :port port :frame-manager frame-manager :process-name process-name :package package))
