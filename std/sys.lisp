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

(defvar *interactive* t
  "When non-nil (the default) specifies that this is an interactive REPL session
and we may query the user for input.")

(define-symbol-macro .i sb-ext:*inspected*)

(defun hooks ()
  (list sb-ext:*init-hooks*
        sb-ext:*after-gc-hooks*
        sb-ext:*save-hooks*
        sb-ext:*exit-hooks*))

(defparameter *default-arena-size* (* 10 1024 1024 1024))

(defparameter *default-heap-size* (ash 1 16))

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

(defun list-all-symbols ()
  (let ((r)) 
    (dolist (p (list-all-packages) r) 
      (appendf r (list-package-symbols p)))))

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

(defmacro do-internal-symbols ((var package) &body forms)
  (std/sym:with-gensyms (state)
    `(do-symbols (,var ,package)
       (multiple-value-bind (,var ,state)
	   (find-symbol (symbol-name ,var) ,package)
	 (when (eq ,state :internal)
	   ,@forms)))))

(defun standard-symbol-names (&optional test)
  (package-symbol-names :common-lisp test))

(defun revive-image (&key (lisp-interaction uiop:*lisp-interaction*)
                          (restore-hook uiop:*image-restore-hook*)
                          (prelude uiop:*image-prelude*)
                          (entry-point uiop/image:*image-entry-point*)
                          (if-already-restored '(cerror "Revive image anyway")))
  (uiop:restore-image :lisp-interaction lisp-interaction :restore-hook restore-hook :prelude prelude
                      :entry-point entry-point :if-already-restored if-already-restored))

;;; Remove all symbols from all packages, storing them in weak pointers,
;;; then collect garbage, and re-intern all symbols that survived GC.
;;; Any symbol satisfying PREDICATE will be strongly referenced during GC
;;; so that it doesn't disappear, regardless of whether it appeared unused.
(in-package :sb-impl)
(defun std/sys:shake-packages (predicate &key print verbose query)
  (declare (function predicate))
  (let (list)
    (flet ((weaken (table accessibility)
             (let ((cells (symtbl-cells table))
                   (result))
               (dovector (x cells)
                 (when (symbolp x)
                   (if (funcall predicate x accessibility)
                       (push x result) ; keep a strong reference to this symbol
                       (push (cons (string x) (make-weak-pointer x)) result))))
               (fill cells 0)
               (resize-symbol-table table 0 'intern)
               result)))
      (dolist (package (list-all-packages))
        ;; Never discard standard symbols
        (unless (eq package sb-int:*cl-package*)
          (push (list* (weaken (package-internal-symbols package) :internal)
                       (weaken (package-external-symbols package) :external)
                       package)
                list))))
    (gc :gen 7)
    (when query
      (sb-ext:search-roots query :criterion :static))
    (let ((n-dropped 0))
      (flet ((reintern (symbols table package access)
               (declare (ignore package))
               (dolist (item symbols)
                 (if (symbolp item)
                     (add-symbol table item 'intern)
                     (let ((symbol (weak-pointer-value (cdr item))))
                       (cond (symbol
                              (add-symbol table symbol 'intern))
                             (t
                              (when print
                                (format t "  (~a)~A~%" access (car item)))
                              (incf n-dropped))))))))
        (loop for (internals externals . package) in list
              do (when print
                   (format t "~&Package ~A~%" package))
                 (reintern internals (package-internal-symbols package)
                           package #\i)
                 (reintern externals (package-external-symbols package)
                           package #\e))
        (when verbose
          (format t "~&Dropped ~D symbols~%" n-dropped))
        (force-output)))))

(in-package :std/sys)
;; TODO
(defun save-lisp-tree-shake-and-die (path &rest args)
  "A naive tree-shaker for lisp."
  ;; https://gist.github.com/burtonsamograd/f08f561264ff94391300
    (loop repeat 10
          do (sb-ext:gc :full t))
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

(defun save-shared-objects (objects)
  "Set the DONT-SAVE slot of OBJECTS to T."
  (mapcar (lambda (obj) (setf (sb-alien::shared-object-dont-save obj) nil)) objects))

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

(defmacro without-fp-traps (() &body body)
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body))


;; https://www.intel.com/content/dam/develop/public/us/en/documents/10tb-24-breakthrough-aes-performance-with-intel-aes-new-instructions-final-secure.pdf

;; ncycles=(tscend-tscstart/i)

;; based on sb-simd-internals and https://kurohuku.blogspot.com/2009/11/sbclcpuid.html

;; also see https://github.com/jdmccalpin/low-overhead-timers/blob/master/low_overhead_timers.c

(defun cpuid (eax &optional (ecx 0))
  "Call the CPUID instruction with supplied 32-bit values for EAX and ECX
regs. Returns 4 values containing the regs RAX RBX RCX and RDX respectively."
  (declare ((unsigned-byte 32) eax ecx))
  (sb-vm::%cpu-identification eax ecx))

(defun word-byte-list (n)
  (list
   (ldb (byte 8 0) n)
   (ldb (byte 8 8) n)
   (ldb (byte 8 16) n)
   (ldb (byte 8 24) n)))

(macrolet ((%with-cpuid (n &body body) 
             `(multiple-value-bind (a b c d) (cpuid ,n) 
                ,@body)))
  (defun cpu-vendor ()
    (%with-cpuid 0
     (declare (ignore a))
     (coerce
      (mapcan
       #'(lambda (n)
	   (mapcar #'code-char (word-byte-list n)))
       (list b d c))
      'string)))
  ;; this is the same as MACHINE-VERSION
  (defun cpu-brand ()
    (with-output-to-string (s)
      (dolist (n '#.(mapcar #'(lambda (x)
			       (coerce x '(unsigned-byte 32)))
		           (list #x80000002 #x80000003 #x80000004)))
        (declare ((unsigned-byte 32) n))
        (%with-cpuid n
	 (dolist (word (list a b c d))
	   (dolist (code (word-byte-list word))
	     (unless (zerop code)
	       (write-char (code-char code) s)))))))))

;; from stmx
(declaim (ftype (function () boolean) transaction-supported-p lock-elision-supported-p))

(defun lock-elision-supported-p ()
  "Test for HLE, i.e. hardware lock elision.
HLE is supported if (cpuid 7) returns ebx with bit 4 set.  If a processor does
not support HLE, it will ignore the new assembler instruction prefixes
XACQUIRE and XRELEASE.

As of June 2013, the only x86-64 CPUs supporting HLE are: * Intel Core i5 4570
* Intel Core i5 4670 * Intel Core i7 4770 Beware: at the time of writing all
the known K models, as for example Intel Core i7 4770K, do **NOT** support
HLE."

  (let ((max-cpuid (cpuid 0)))
    (when (>= max-cpuid 7)
      (let ((ebx (nth-value 1 (cpuid 7))))
        (not (zerop (logand ebx #x10)))))))

(defun transaction-supported-p ()
  "Test for RTM, i.e. hardware memory transactions.
RTM is supported if (cpuid 7) returns ebx with bit 11 set.  If a processor
does not support HLE, trying to execute the new assembler instructions XBEGIN,
XEND, XABORT and XTEST will generate faults.

As of June 2013, the only x86-64 CPUs supporting RTM are:
* Intel Core i5 4570
* Intel Core i5 4670
* Intel Core i7 4770

Beware: at the time of writing all the known K models, as for example Intel
Core i7 4770K, do **NOT** support RTM."
    (let ((max-cpuid (cpuid 0)))
      (when (>= max-cpuid 7)
        (let ((ebx (nth-value 1 (cpuid 7))))
          (not (zerop (logand ebx #x800)))))))

(defparameter %little-endian nil)

(defun little-endian-p ()
  "Return T if the current platform is little-endian else NIL."
  #+(or :x86 :x86-64 :little-endian) t
  #+(or :PPC :POWERPC :big-endian) nil
  #-(or :x86 :x86-64 :little-endian :ppc :powerpc :big-endian)
  %little-endian)

(defun 64-bit-p () 
  "Return T on a 64-bit platform else NIL."
  #+x86-64 t)
(defun 32-bit-p () 
  "Return T on a 64-bit platform else NIL."
  #+x86 t)

(defun register-project-directory (path &optional (asdf t))
  "Add PATH to QL:*LOCAL-PROJECT-DIRECTORIES* and ASDF:*CENTRAL-REGISTRY* (as
long as ASDF is non-nil)."
  #+quicklisp (pushnew path ql:*local-project-directories*)
  (when asdf (pushnew path asdf:*central-registry*)))

;;; Time
(defun get-real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun %time-remaining (start timeout) (- timeout (- (get-real-time-seconds) start)))

(defmacro! with-countdown (o!time &body body)
  (with-gensyms (start)
    `(let ((,start (get-real-time-seconds)))
       (flet ((time-remaining () (std/sys::%time-remaining ,start ,g!time)))
         (declare (inline time-remaining))
         ,@body))))

;;; Logical Pathnames
(defun logical-host-names ()
  "Print a list of currently available logical hosts."
  (map 'list (lambda (x) (slot-value x 'sb-impl::name)) *logical-hosts*))

(defmacro define-logical-pathname (host path &rest translations)
  (unless (null path)
    (setf translations 
	  (append `((,(format nil "~A" host) ,path)) translations)))
  `(setf (logical-pathname-translations ,host)
         ;; eval second element only
	 ',(mapcar (lambda (x)
                     (setf (cadr x) (eval (cadr x)))
                     x)
                   translations)))

(define-logical-pathname "STASH" "/opt/stash/"
  ("**;*.*.*" "/opt/stash/**/*.*"))
(define-logical-pathname "STORE" "/opt/store/"
  ("**;*.*.*" "/opt/store/**/*.*"))
(define-logical-pathname "SCRATCH" "/opt/scratch/"
  ("**;*.*.*" "/opt/scratch/**/*.*"))
;; redefine the sys table
(define-logical-pathname "SYS" "/usr/local/lib/sbcl/"
  ("SRC;**;*.*.*" #P"/usr/local/src/sbcl/src/**/*.*")
  ("CONTRIB;**;*.*.*"
   #P"/usr/local/src/sbcl/contrib/**/*.*")
  ("OUTPUT;**;*.*.*"
   (translate-logical-pathname "STASH:OUTPUT;sbcl;**;*.*.*"))
  ("TMP;**;*.*.*" "/tmp/**/*.*"))
