;;; std/sys.lisp @ 2023-10-14.03:28:40 -*- mode: lisp; -*-

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

;; TODO 2024-05-09: 
;; (defun decode-all-debug-data ()
;;   (dolist (code (sb-vm:list-allocated-objects :all :type sb-vm:code-header-widetag))
;;     (let ((info (sb-kernel:%code-debug-info code)))
;;       (when (typep info 'sb-c::compiled-debug-info)
;;         (let ((fun-map (sb-di::get-debug-info-fun-map
;;                         (sb-kernel:%code-debug-info code))))
;;           (loop for i from 0 below (length fun-map) by 2 do
;;             (let ((cdf (aref fun-map i)))
;;               (sb-di::debug-fun-lambda-list
;;                (sb-di::make-compiled-debug-fun cdf code))))))
;;       (print info))))

(defun forget-shared-objects ()
  "Set the DONT-SAVE slot of all objects in SB-SYS:*SHARED-OBJECTS* to T."
  (mapcar (lambda (obj) (setf (sb-alien::shared-object-dont-save obj) t)) sb-sys:*shared-objects*))
