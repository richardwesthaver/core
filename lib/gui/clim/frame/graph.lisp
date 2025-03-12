;;; graph.lisp --- Graph-based Frames

;; 

;;; Code:
(in-package :gui/clim/frame)

(define-application-frame graph-frame () ()
  (:menu-bar nil)
  (:panes (graph
           (clim:scrolling ()
             (make-pane 'clim:application-pane
		        :width 500
		        :height 800
		        :background clim:+lightgray+
		        :foreground clim:+black+
		        :display-function 'generate-graph
		        :display-time t)))))

(defvar *graph-root* 'id:id)

(defun generate-graph (frame pane)
  (declare (ignore frame))
  (format-graph-from-roots
   (list (find-class *graph-root*))
   (lambda (object stream)
     (present (class-name object) (presentation-type-of object) :stream stream))
   #'sb-mop:class-direct-subclasses
   :stream pane
   ;; :orientation :vertical
   ;; :graph-type :digraph
   :merge-duplicates t))

(defun find-graph-node (record)
  "Searches upward until a graph node parent of the supplied output record is found."
  (loop for current = record then (output-record-parent current)
        while current
        when (graph-node-output-record-p current)
          do (return current)))

(defun redisplay-edges (graph edges)
  (dolist (edge edges)
    (climi::layout-edge-1 graph (climi::from-node edge) (climi::to-node edge))))

(defun node-edges (node)
  (append (hash-table-values (slot-value node 'climi::edges-from))
          (hash-table-values (slot-value node 'climi::edges-to))))

(defun view-graph (&optional (class *graph-root*))
  (setq *graph-root* class)
  (clim:find-application-frame 'graph-frame))

(defun node-and-edges-region (node edges)
  (reduce #'region-union edges :key #'copy-rectangle
                               :initial-value (copy-rectangle node)))

(defun copy-rectangle (region)
  (with-bounding-rectangle* (x0 y0 x1 y1) region
    ;; We use this rectangle to clear an area on the sheet which only
    ;; makes sense for integer coordinates.
    (make-rectangle* (floor x0) (floor y0) (ceiling x1) (ceiling y1))))

(define-graph-frame-command (com-drag-node)
    ((record t)
     (offset-x 'real :default 0)
     (offset-y 'real :default 0))
  (with-application-frame (frame)
    (let* ((stream (clim:get-frame-pane frame 'graph))
           (node-record (find-graph-node record))
           (edge-records (node-edges node-record))
           (graph-record (output-record-parent node-record))
           (erase-region))
      (assert (typep graph-record 'graph-output-record))
      (drag-output-record
       stream node-record
       :feedback (lambda (record stream old-x old-y x y mode)
                   (declare (ignore old-x old-y))
                   (ecase mode
                     (:erase
                      ;; Capture current regions before modifying the
                      ;; output records.
                      (setf erase-region
                            (node-and-edges-region record edge-records))
                      ;; Remove contents (i.e. lines) of edge output
                      ;; records. This does not repaint anything. To
                      ;; account for that, we include ERASE-REGION in
                      ;; the :DRAW clause.
                      (map nil #'clear-output-record edge-records))
                     (:draw
                      ;; Reposition the node record (this does not
                      ;; automatically replay the record).
                      (setf (output-record-position record)
                            (values (- x offset-x) (- y offset-y)))
                      ;; Regenerate child records of the edge records
                      ;; for the changed node position (without drawing
                      ;; since we will draw everything at once as a
                      ;; final step).
                      (with-output-recording-options (stream :record t :draw nil)
                        (redisplay-edges graph-record edge-records))
                      ;; Repaint all affected areas. This also replays
                      ;; the modified node and edge output records.
                      (dispatch-repaint
                       stream (region-union (or erase-region +nowhere+)
                                            (node-and-edges-region
                                             record edge-records))))))
       :finish-on-release t :multiple-window nil))))

(define-presentation-to-command-translator record-dragging-translator
    (t com-drag-node graph-frame
       :tester ((object presentation)
                (find-graph-node presentation)))
    (object presentation x y)
  (multiple-value-bind (old-x old-y) (output-record-position presentation)
    (list presentation (- x old-x) (- y old-y))))
