;;; pkg.lisp --- CLIM Packages

;;

;;; Code: 
(pkg:defpkg :gui/clim
  (:use :clim :clim-lisp :clim-extensions)
  (:recycle :clim :clim-lisp)
  (:shadowing-import-from :std)
  (:shadowing-import-from :cl)
  (:export :define-application-frame
   :make-pane :define-presentation-to-command-translator
   :present :presentation-type-of 
   :output-record-parent :graph-node-output-record-p :region-union :format-graph-from-roots
   :with-bounding-rectangle* :make-rectangle* :with-application-frame :+nowhere+
   :find-pane-named :graph-output-record :drag-output-record :define-command-table
   :clear-output-record :output-record-position :with-output-recording-options :dispatch-repaint
   :*application-frame* :make-application-frame :run-frame-top-level))

(pkg:defpkg :gui/clim/layout 
  (:use :cl :std :gui/clim) 
  (:use-reexport :clim-tab-layout))

(pkg:defpkg :gui/clim/frame 
  (:use :cl :std :gui/clim)
  (:use-reexport :clim-tab-layout) 
  (:import-from :clim :define-application-frame :make-pane
   :present :presentation-type-of :make-application-frame :application-frame
   :run-frame-top-level
   :output-record-parent :graph-node-output-record-p :region-union :format-graph-from-roots
   :with-bounding-rectangle* :make-rectangle* :with-application-frame :+nowhere+
   :find-pane-named :graph-output-record :drag-output-record :define-command-table
   :clear-output-record :output-record-position :with-output-recording-options :dispatch-repaint
   :define-presentation-to-command-translator))
   
(pkg:defpkg :gui/clim/dbg 
  (:use :cl :std :gui/clim :gui/clim/frame)
  (:import-from :clim-debugger :clim-debugger :the-condition :returned-restart :backtrace :condition-info :shown-frames :inspectable :change-space-requirements :frame-panes :active-frame :*pointer-documentation-output* :frame-exit :stack-frame :frame-no :+minimized-stack-frame-view+ :view :+maximized-stack-frame-view+ :frame-current-layout)
  (:use-reexport :clim-debugger)
  (:export 
   :clouseau-inspect
   :install-clim-debugger))

(pkg:defpkg :gui/clim/repl 
  (:use :cl :std :gui/clim :cli/repl) 
  (:use-reexport :clim-listener)
  (:export :run-clim-listener))
