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

(defvar *default-arena-size* (* 10 1024 1024 1024))

(defun current-lisp-implementation ()
  "Return the current lisp implemenation as a list: (TYPE VERSION FEATURES)"
  (list 
   (lisp-implementation-type)
   (lisp-implementation-version)
   *features*))

(defun current-machine ()
  "Return the current machine spec as a list: (HOST TYPE VERSION)"
  (list
   (machine-instance)
   (machine-type)
   (machine-version)))

(defun list-package-symbols (&optional (pkg *package*))
  (loop for s being the external-symbol of pkg
        collect s))

(defun package-symbols (&optional (package *package*) test)
  (let ((symbols))
    (do-external-symbols (symbol package)
      (if test
          (when (funcall test symbol)
            (push symbol symbols))
          (push symbol symbols)))
    symbols))

(defun package-symbol-names (&optional (package *package*) test)
  (sort (mapcar (lambda (x) (string-downcase (symbol-name x)))
                (package-symbols package test))
        #'string<))

(defun standard-symbol-names (test)
  (package-symbol-names :common-lisp test))

(defun append-logical-hosts (&rest hosts)
  "Reinitialize SB-IMPL::*LOGICAL-HOSTS* with a freshly allocated vector
consisting of the old contents appended to the new."
  (setq *logical-hosts*
        (concatenate 'vector hosts *logical-hosts*)))

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

(defun forget-shared-object (name)
  (setf (sb-alien::shared-object-dont-save
         (find name sb-sys:*shared-objects*
               :key 'sb-alien::shared-object-namestring
               :test 'string-equal))
        t))

(defun forget-shared-objects (&optional (objects sb-sys:*shared-objects*))
  "Set the DONT-SAVE slot of all objects in SB-SYS:*SHARED-OBJECTS* to T."
  (mapcar (lambda (obj) (setf (sb-alien::shared-object-dont-save obj) t)) objects))

(defun compile-lisp (name &key force save make package compression verbose version callable-exports executable (toplevel #'sb-impl::toplevel-init) forget save-runtime-options root-structures (purify t))
  (pkg:with-package (or package *package*)
    (asdf:compile-system name :force force :verbose verbose :version version)
    (when make
      (apply 'asdf:make name (unless (eq t make) make)))
    (when forget
      (forget-shared-objects (unless (eq t forget) forget)))
    (when save
      (when (probe-file save)
        (delete-file save))
      (sb-ext:save-lisp-and-die save :executable executable
                                     :toplevel toplevel
                                     :callable-exports callable-exports
                                     :save-runtime-options save-runtime-options
                                     :root-structures root-structures
                                     :purify purify
                                     :compression compression))))
