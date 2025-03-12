;;; repl.lisp --- CLIM REPL

;; Based on CLIM-LISTENER

;;; Code:
(in-package :gui/clim/repl)

(defun run-clim-listener (&key new-process (debugger t)
                               (width 790) (height 550) port frame-manager 
                               (process-name "Listener") (package :std-user))
  (run-listener :new-process new-process :debugger debugger :width width :height height :port port :frame-manager frame-manager :process-name process-name :package package))
