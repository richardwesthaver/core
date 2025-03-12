;;; dbg.lisp --- Debug interface based on CLIM-DEBUGGER

;; 

;;; Code:
(in-package :gui/clim/dbg)

(defun clouseau-inspect (obj &key new-process (handle-errors t))
  (clouseau:inspect obj :new-process new-process :handle-errors handle-errors))

(defun install-clim-debugger ()
  (install-debugger))

(define-application-frame dbg ()
  ((condition        :initform nil :accessor the-condition)
   (returned-restart :initform nil :accessor returned-restart))
  (:pointer-documentation t)
  (:panes (debugger-pane (let ((condition (the-condition *application-frame*)))
                           (make-pane 'clim-debugger::debugger-pane
                                      :root condition
                                      :condition-info condition
                                      :end-of-line-action :allow
                                      :end-of-page-action :scroll
                                      :width :compute
                                      :height :compute
                                      :background clim:+lightgray+
                                      :foreground clim:+black+)))
          (interactor :interactor
                      :height 80
                      :background clim:+lightgray+
                      :foreground clim:+black+))
  (:command-table (dbg :inherit-from (clouseau:inspector-command-table))))

(define-presentation-to-command-translator more-backtraces
    (clim-debugger::more-type com-more clim-debugger :gesture :select)
    (object)
  (list))

(defmethod frame-standard-output ((frame dbg))
  (or (find-pane-named frame 'interactor)
      (call-next-method)))

(defun dbg (condition me-or-my-encapsulation)
  (let ((debugger-frame (make-application-frame 'dbg)))
    (swank-backend::call-with-debugging-environment
     (lambda ()
       (unwind-protect
            (setf (the-condition debugger-frame)
                  (clim-debugger::make-debugger-info
                   condition (compute-restarts) (clim-debugger::compute-backtrace 0 nil)))
         (run-frame-top-level debugger-frame)
         (if-let ((restart (returned-restart debugger-frame)))
           (let ((*debugger-hook* me-or-my-encapsulation))
             (invoke-restart-interactively restart))
           (abort)))))))

(defun install-dbg ()
  (setf *debugger-hook* #'dbg
        sb-ext:*invoke-debugger-hook* #'dbg))

(defun run-dbg-frame ()
  (run-frame-top-level (make-application-frame 'dbg)))

;;; Commands
(define-dbg-command (com-more :name "More backtraces"
                                        :keystroke :more)
    ()
  (let ((pane (find-pane-named *application-frame* 'debugger-pane)))
    (setf #1=(shown-frames pane)
          (min (+ #1# 10) (length (backtrace (condition-info pane)))))))

(define-dbg-command (com-invoke-inspector :name "Inspect in new frame")
    ((obj 'inspectable :gesture (:select
                                 :documentation "Inspect in new frame"
                                 :pointer-documentation "Inspect in new frame")))
  (clouseau:inspect obj :new-process t))

(define-dbg-command (com-refresh :name "Refresh" :menu t
                                           :keystroke #\r)
    ()
  (change-space-requirements (frame-panes *application-frame*)))

(define-dbg-command (com-next :keystroke :next)
    ()
  (let* ((pane (find-pane-named *application-frame* 'debugger-pane))
         (shown-frames (shown-frames pane)))
    (incf (active-frame pane))
    (when (= (active-frame pane) shown-frames)
      (com-more))
    (when (= (active-frame pane) shown-frames)
      (decf (active-frame pane)))))

(define-dbg-command (com-prev :keystroke :prev)
    ()
  (let* ((pane (find-pane-named *application-frame* 'debugger-pane)))
    (setf (active-frame pane) (max (1- (active-frame pane)) 0))))

(define-dbg-command (com-eval :name "Eval in frame" :menu t
                                        :keystroke :eval)
    ((form 'clim:string))
  (let* ((dbg-pane (find-pane-named *application-frame* 'debugger-pane))
         (active-frame (active-frame dbg-pane)))
    (format *pointer-documentation-output*
            (swank:eval-string-in-frame
             form active-frame (swank-backend:frame-package active-frame) 10 80))))

(define-dbg-command (com-quit :name "Quit" :menu t
                                        :keystroke :exit) ()
  (frame-exit *application-frame*))

(define-dbg-command (com-invoke-restart :name "Invoke restart")
    ((restart 'restart :gesture :select))
  (setf (returned-restart *application-frame*) restart)
  (frame-exit *application-frame*))

(define-dbg-command (com-toggle-stack-frame-view
                               :name "Toggle stack frame view")
    ((stack-frame 'stack-frame :gesture (:select :documentation "Toggle stack frame view")))

  (let ((dbg-pane (find-pane-named *application-frame* 'debugger-pane)))
    (setf (active-frame dbg-pane) (frame-no stack-frame)))

  (if (eq +minimized-stack-frame-view+ (view stack-frame))
      (setf (view stack-frame) +maximized-stack-frame-view+)
      (setf (view stack-frame) +minimized-stack-frame-view+))
  (change-space-requirements (frame-panes *application-frame*)))

(define-dbg-command (com-toggle-active-frame-view
                               :keystroke :toggle
                               :name "Toggle active")
    ()
  (let ((dbg-pane (find-pane-named *application-frame* 'debugger-pane)))
    (com-toggle-stack-frame-view
     (nth (active-frame dbg-pane) (backtrace (condition-info dbg-pane))))))

;; (define-dbg-command (clim-toggle-interactor
;;                                :name      "Toggle interactor"
;;                                :keystroke (#\i :control))
;;     ()
;;   (let ((frame *application-frame*))
;;     (setf (frame-current-layout frame)
;;           (case (frame-current-layout frame)
;;             (without-interactor 'with-interactor)
;;             (with-interactor    'without-interactor)))))
