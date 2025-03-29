;;; gui.lisp --- Organ GUI

;; 

;;; Code:
(defpackage :organ/gui
  (:use :cl :std :organ :organ/graph :gui/clim :gui/clim/frame)
  (:import-from :gui/clim/frame :graph-frame)
  (:import-from :gui/clim :vertically :define-application-frame :scrolling :find-application-frame)
  (:export :org-graph-view :org-node-view :org-edge-view :display-org-graph :org-graph-frame))
(in-package :organ/gui)

(defun org-graph-view () (find-application-frame 'org-graph-frame))

(define-application-frame org-graph-frame (graph-frame) ()
  (:panes 
   (graph
    (scrolling ()
      (make-pane 'clim:application-pane
		 :background clim:+gray21+
		 :foreground clim:+oldlace+
		 :display-function 'generate-org-graph
		 :display-time t)))
    (repl
     (make-pane 'clim:interactor-pane
                :height 80
		:background clim:+black+
		:foreground clim:+blanchedalmond+)))
  (:layouts
   (default (vertically ()
              graph
              repl))))

(defun generate-org-graph (frame pane)
  (declare (ignore frame))
  (let ((edges (organ/graph::org-graph-edges organ/graph::*org-graph*)))
    (format-graph-from-roots
     (organ/graph::org-graph-nodes organ/graph::*org-graph*)
     (lambda (o s)
       (present (name o) (presentation-type-of o) :stream s))
     (lambda (x)
       (loop for edge in edges if (ignore-errors (or 
						  (uuid:uuid= (id:id x) (organ/graph::edge-in edge))
						  (uuid:uuid= (id:id x) (organ/graph::edge-out edge))))
	     collect edge))
     :stream pane)))
