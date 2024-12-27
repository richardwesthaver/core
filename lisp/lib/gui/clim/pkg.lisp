;;; pkg.lisp --- CLIM Packages

;;

;;; Code: 
#+dbg (pkg:defpkg :gui/clim/dbg (:use :cl :std :gui/clim) (:use-reexport :clim-debugger))
#+repl (pkg:defpkg :gui/clim/repl (:use :cl :std :gui/clim :cli/repl) (:use-reexport :clim-listener))
(pkg:defpkg :gui/clim/layout 
  (:use :cl :std :gui/clim) 
  (:use-reexport :clim-tab-layout))

(pkg:defpkg :gui/clim/frame 
  (:use :cl :std :gui/clim) 
  (:use-reexport :clim-tab-layout) 
  (:import-from :clim :define-application-frame :make-pane
   :present :presentation-type-of 
   :output-record-parent :graph-node-output-record-p :region-union :format-graph-from-roots
   :with-bounding-rectangle* :make-rectangle* :with-application-frame :+nowhere+
   :find-pane-named :graph-output-record :drag-output-record :define-command-table
   :clear-output-record :output-record-position :with-output-recording-options :dispatch-repaint
   :define-presentation-to-command-translator))
   
(in-package :gui/clim)
