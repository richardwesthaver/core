;;; std/sys.lisp --- Lisp System Utilities

;;

;;; Code:
(in-package :std/sys)

;;; Introspection
;; (reexport-from :sb-introspect
;; 	       :include '(:function-lambda-list :lambda-list-keywords :lambda-parameters-limit
;; 			  :method-combination-lambda-list :deftype-lambda-list
;; 			  :primitive-object-size :allocation-information
;; 			  :function-type
;; 			  :who-specializes-directly :who-specializes-generally
;; 			  :find-function-callees :find-function-callers))

;; sys
;; sb-sys:*linkage-info* *machine-version* *runtime-dlhandle* *periodic-polling-function*
;; *periodic-polling-period* io-timeout nlx-protect serve-event os-deinit os-exit with-deadline dlopen-or-lose deallocate-system-memory

(defun current-lisp-implementation ()
  "Return the current lisp implemenation as a cons: (TYPE VERSION)"
  (list 
   (lisp-implementation-type) 
   (lisp-implementation-version)
   *features*))

;; TODO
(defun save-lisp-tree-shake-and-die (path &rest args)
  "A naive tree-shaker for lisp."
  (sb-ext:gc :full t)
  (apply #'sb-ext:save-lisp-and-die path args))

(defun save-lisp-and-live (filename completion-function restart &rest args)
  (flet ((restart-sbcl ()
           (sb-debug::enable-debugger)
           (setf sb-impl::*descriptor-handlers* nil)
           (funcall restart)))
    ;; fork it - assumes only one thread is running
    (multiple-value-bind (pipe-in pipe-out) (sb-posix:pipe)
      (let ((pid (sb-posix:fork)))
        (cond ((= pid 0) ;; make simple-restart core
               (sb-posix:close pipe-in)
               (sb-debug::disable-debugger)
               (apply #'sb-ext:save-lisp-and-die filename
                      (append
                       (list :toplevel #'restart-sbcl)
                       args)))
              (t
               (sb-posix:close pipe-out)
               (sb-sys:add-fd-handler
                pipe-in :input
                (lambda (fd)
                  (sb-sys:invalidate-descriptor fd)
                  (sb-posix:close fd)
                  (multiple-value-bind (rpid status) (sb-posix:waitpid pid 0) ;; wait for master
                    (assert (= pid rpid))
                    (assert (sb-posix:wifexited status))
                    (funcall completion-function
                             (zerop (sb-posix:wexitstatus status))))))))))))

(defparameter *gc-logfile* #P"gc.log")

(defun enable-gc-logfile (&optional (file *gc-logfile*))
  (setf (sb-ext:gc-logfile) file))

(length (sb-di::list-allocated-objects :dynamic :test #'stringp))

(defun forget-shared-object (name)
  (setf (sb-alien::shared-object-dont-save
         (find name sb-sys:*shared-objects*
               :key 'sb-alien::shared-object-namestring
               :test 'string-equal))
        t))

(defun forget-shared-objects ()
  "Set the DONT-SAVE slot of all objects in SB-SYS:*SHARED-OBJECTS* to T."
  (mapcar (lambda (obj) (setf (sb-alien::shared-object-dont-save obj) t)) sb-sys:*shared-objects*))
