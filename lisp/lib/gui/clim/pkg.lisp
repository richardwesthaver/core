;;; pkg.lisp --- CLIM Packages

;; 

;;; Code:
#+dbg
(pkg:defpkg :gui/clim/dbg
  (:use :cl :std :gui/clim)
  (:use-reexport :clim-debugger))

(pkg:defpkg :gui/clim/layout
  (:use :cl :std :gui/clim)
  (:use-reexport :clim-tab-layout))

(pkg:defpkg :gui/clim/frame
  (:use :cl :std :gui/clim)
  (:use-reexport :clim-tab-layout)
  (:import-from :clim :define-application-frame :make-pane :present :presentation-type-of :format-graph-from-roots :output-record-parent :graph-node-output-record-p :region-union :with-bounding-rectangle* :make-rectangle* :with-application-frame :find-pane-named :graph-output-record :drag-output-record :clear-output-record :output-record-position :with-output-recording-options :dispatch-repaint :+nowhere+ :define-presentation-to-command-translator :define-command-table))
