;;; swank-presentation-streams.lisp --- Presentation Streams

;; Streams that allow attaching object identities to portions of output

;; Authors: Alan Ruttenberg  <alanr-l@mumble.net>
;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Helmut Eller  <heller@common-lisp.net>

;;; Code:
(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-presentations))

;; This file contains a mechanism for printing to the slime repl so
;; that the printed result remembers what object it is associated
;; with. This extends the recording of REPL results.

;; There are two methods:

;; 1. Depends on the ilisp bridge code being installed and ready to
;;    intercept messages in the printed stream. We encode the
;;    information with a message saying that we are starting to print
;;    an object corresponding to a given id and another when we are
;;    done. The process filter notices these and adds the necessary
;;    text properties to the output.

;; 2. Use separate protocol messages :presentation-start and
;;    :presentation-end for sending presentations.

;; We only do this if we know we are printing to a slime stream,
;; checked with the method slime-stream-p. Initially this checks for
;; the knows slime streams looking at *connections*. In cmucl, sbcl, and
;; openmcl it also checks if it is a pretty-printing stream which
;; ultimately prints to a slime stream.

;; Method 1 seems to be faster, but the printed escape sequences can 
;; disturb the column counting, and thus the layout in pretty-printing.
;; We use method 1 when a dedicated output stream is used.  

;; Method 2 is cleaner and works with pretty printing if the pretty
;; printers support "annotations".  We use method 2 when no dedicated
;; output stream is used.

;; Control
(defvar *enable-presenting-readable-objects* t
  "set this to enable automatically printing presentations for some
subset of readable objects, such as pathnames."  )

;; doing it

(defmacro presenting-object (object stream &body body)
  "What you use in your code. Wrap this around some printing and that text will
be sensitive and remember what object it is in the repl"
  `(presenting-object-1 ,object ,stream #'(lambda () ,@body)))

(defmacro presenting-object-if (predicate object stream &body body)
  "What you use in your code. Wrap this around some printing and that text will
be sensitive and remember what object it is in the repl if predicate is true"
  (let ((continue (gensym)))
    `(let ((,continue #'(lambda () ,@body)))
       (if ,predicate
	   (presenting-object-1 ,object ,stream ,continue)
	   (funcall ,continue)))))

(let ((last-stream nil)
      (last-answer nil))
  (defun slime-stream-p (stream)
    "Check if stream is one of the slime streams, since if it isn't we
don't want to present anything.
Special return values: 
:REPL-RESULT -- Output ends up on the :repl-results target.
"
    (if (eq last-stream stream)
	last-answer
	(progn
	  (setq last-stream stream)
	  (if (eq stream t) 
	      (setq stream *standard-output*))
	  (setq last-answer 
		(or (let ()
		      (declare (notinline sb-pretty::pretty-stream-target))
		      (and (typep stream (find-symbol "PRETTY-STREAM" 'sb-pretty))
                           (find-symbol "ENQUEUE-ANNOTATION" 'sb-pretty)
                           (slime-stream-p (sb-pretty::pretty-stream-target stream))))
		    (loop for connection in *connections*
			  thereis (or (eq stream (connection.socket-io connection))
				      (eq stream (connection.user-output connection))
				      (eq stream (connection.user-io connection))
				      (and (eq stream (connection.repl-results connection))
					   :repl-result)))))))))

(defun can-present-readable-objects (&optional stream)
  (declare (ignore stream))
  *enable-presenting-readable-objects*)

;; If we are printing to an XP (pretty printing) stream, printing the
;; escape sequences directly would mess up the layout because column
;; counting is disturbed.  Use "annotations" instead.
(defun write-annotation (stream function arg)
  (let ((enqueue-annotation
	  (find-symbol "ENQUEUE-ANNOTATION" 'sb-pretty)))
    (if (and enqueue-annotation
	     (typep stream (find-symbol "PRETTY-STREAM" 'sb-pretty)))
	(funcall enqueue-annotation stream function arg)
	(funcall function arg stream nil))))

(defstruct presentation-record 
  (id)
  (printed-p)
  (target))

(defun presentation-start (record stream truncatep) 
  (unless truncatep
    ;; Don't start new presentations when nothing is going to be
    ;; printed due to *print-lines*.
    (let ((pid (presentation-record-id record))
	  (target (presentation-record-target record)))
      (case target
	(t
	 (finish-output stream)
	 (send-to-emacs `(:presentation-start ,pid ,target)))))
    (setf (presentation-record-printed-p record) t)))

(defun presentation-end (record stream truncatep)
  (declare (ignore truncatep))
  ;; Always end old presentations that were started.
  (when (presentation-record-printed-p record)
    (let ((pid (presentation-record-id record))
	  (target (presentation-record-target record)))
      (case target
	(t
	 (finish-output stream)
	 (send-to-emacs `(:presentation-end ,pid ,target)))))))

(defun presenting-object-1 (object stream continue)
  "Uses the bridge mechanism with two messages >id and <id. The first one
says that I am starting to print an object with this id. The second says I am finished"
  ;; this declare special is to let the compiler know that *record-repl-results* will eventually be
  ;; a global special, even if it isn't when this file is compiled/loaded.
  (declare (special *record-repl-results*))
  (let ((slime-stream-p 
	  (and *record-repl-results* (slime-stream-p stream))))
    (if slime-stream-p
	(let* ((pid (swank::save-presented-object object))
	       (record (make-presentation-record :id pid :printed-p nil
						 :target (if (eq slime-stream-p :repl-result)
							     :repl-result
							     nil))))
	  (write-annotation stream #'presentation-start record)
	  (multiple-value-prog1
	      (funcall continue)
	    (write-annotation stream #'presentation-end record)))
	(funcall continue))))

(defun present-repl-results-via-presentation-streams (values)
  ;; Override a function in swank.lisp, so that 
  ;; nested presentations work in the REPL result.
  (let ((repl-results (connection.repl-results *emacs-connection*)))
    (flet ((send (value)
	     (presenting-object value repl-results
	       (prin1 value repl-results))
	     (terpri repl-results)))
      (if (null values)
	  (progn 
	    (princ "; No value" repl-results)
	    (terpri repl-results))
	  (mapc #'send values)))
    (finish-output repl-results)))

(in-package :swank)

(defvar *saved-%print-unreadable-object*
  (fdefinition 'sb-impl::%print-unreadable-object))
(defun monkey-patch-stream-printing ()
  (sb-ext:without-package-locks
    (when (eq (fdefinition 'sb-impl::%print-unreadable-object)
	      *saved-%print-unreadable-object*)
      (setf (fdefinition 'sb-impl::%print-unreadable-object)
	    (lambda (object stream &rest args)
	      (presenting-object object stream
                (apply *saved-%print-unreadable-object*
                       object stream args)))))
    (defmethod print-object :around ((object pathname) stream)
      (presenting-object object stream
	(call-next-method)))))

;; Hook into SWANK.
(defslimefun init-presentation-streams ()
  (monkey-patch-stream-printing)
  ;; FIXME: import/use swank-repl to avoid package qualifier.
  (setq swank-repl:*send-repl-results-function*
	'present-repl-results-via-presentation-streams))

(provide :swank-presentation-streams)
