;;; dbg.lisp --- Debug interface based on CLIM-DEBUGGER

;; 

;;; Code:
(in-package :gui/clim/dbg)

(defun clouseau-inspect (obj &key new-process (handle-errors t))
  (clouseau:inspect obj :new-process new-process :handle-errors handle-errors))

(defun install-clim-debugger ()
  (install-debugger))

