;;; plot.lisp --- Plotting CLI Tools

;; gnuplot

;;; Code:
(in-package :cli/tools/plot)

(define-cli-tool :gnuplot)

(defvar *gnuplot-process* nil)

(defun open-gnuplot (&key (gnuplot-binary *gnuplot*)
		          (terminal "wxt")
		          hostname)
  (or *gnuplot-process*
      (progn
	(setf *gnuplot-process* 
              (sb-ext:run-program
	       (if hostname "/usr/bin/ssh" gnuplot-binary) (when hostname (list hostname)) :input :stream :wait nil :output t))
	(when hostname (gnuplot-send "export DISPLAY=:0~%~A~%" gnuplot-binary))
	(gnuplot-send "~%set datafile fortran~%set term ~a~%" terminal)
	*gnuplot-process*)))

(defun close-gnuplot ()
  (when *gnuplot-process*
    (gnuplot-send "quit~%")
    (setf *gnuplot-process* nil)))

(defmacro with-gnuplot-stream ((stream) &rest body)
  `(let ((,stream (sb-ext:process-input
		   (open-gnuplot))))
     (unwind-protect (progn ,@body) (finish-output ,stream))))

(defun gnuplot-send (str &rest args)
  (with-gnuplot-stream (s)
    (apply #'format s str args)))
;;
(defmacro with-gnuplot-term ((stream num &key multiplot (terminal "wxt") output) &rest body)
  (using-gensyms (decl (num output terminal multiplot))
    `(let (,@decl)
       (with-gnuplot-stream (,stream)
	 (format ,stream "set term push~%set term ~a ~a~%" ,terminal ,num)
	 (when ,output (format ,stream "set output '~a'~%" (etypecase ,output (pathname (pathname-name ,output)) (string ,output))))
	 (when ,multiplot (format ,stream "set multiplot~%"))
	 (unwind-protect (progn ,@body)
	   (when ,multiplot (format ,stream "unset multiplot~%"))
	   (when ,output (format ,stream "set output~%"))
	   (format ,stream "set term pop~%"))))))

(define-cli-tool :dot (args &key (output t) input)
  (let ((proc (sb-ext:run-program *dot* args :output output :input input)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (dot-error "DOT (graphviz) command failed: ~A ~A" *dot* args))))

(defun dot-to-svg (file)
  (run-dot `("-Tsvg" ,(format nil "-o~A.svg" (pathname-name file)) ,file)))
