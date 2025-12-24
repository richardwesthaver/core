;;; std/sys.lisp --- Lisp System Utilities

;;

;;; Code:
(in-package :std/sys)
(std-int:in-readtable :std)

;;;_. System Paths
(defun user-config-path () (funcall sb-ext:*userinit-pathname-function*))
(defmethod std/meta:init ((self (eql :user)) &key (load t))
  (when load (load (user-config-path))))

;; These paths may be rebound based on application context.
(defvar *stash* (merge-pathnames ".stash/" (user-homedir-pathname)))
(defvar *store* (merge-pathnames ".store/" (user-homedir-pathname)))

(defun find-stash-directory (&optional (path *default-pathname-defaults*))
  "Find the closest STASH directory - first check local, then user, then
global."
  (or (probe-file (make-pathname :name ".stash/" :defaults path))
      (probe-file #l"USER:STASH;")
      (probe-file #l"STASH:")))

(definline stash-pathname (path)
  (let ((stash (find-stash-directory (or (pathname-directory path) *default-pathname-defaults*))))
    (make-pathname :name (pathname-name path) :type (pathname-type path) :defaults stash)))

(defun find-store-directory (&optional (path *default-pathname-defaults*))
  "Find the closest STORE directory - first check local, then local stash, then
user, then
global."
  (or (probe-file (make-pathname :name ".store/" :defaults path))
      (probe-file (make-pathname :name ".stash/store/" :defaults path))
      (probe-file #l"USER:STORE;")
      (probe-file #l"STORE:")))

(definline store-pathname (path)
  (let ((store (find-stash-directory (or (pathname-directory path) *default-pathname-defaults*))))
    (make-pathname :name (pathname-name path) :type (pathname-type path) :defaults store)))

(definline primitive-type-name-of (obj)
  (primitive-type-name (primitive-type-of obj)))

(defun backend-primitive-type (name)
  (gethash name *backend-primitive-type-names*))

(defvar *interactive* t
  "When non-nil (the default) specifies that this is an interactive REPL session
and we may query the user for input.")

(define-symbol-macro .i sb-ext:*inspected*)

(defun sbcl-hooks ()
  "Return the available SBCL hook symbols as an unevaluated plist."
  '(:init sb-ext:*init-hooks*
    :after-gc sb-ext:*after-gc-hooks*
    :run-gc sb-impl::*run-gc-hooks*
    :compile-component sb-c::*compile-component-hook*
    :macroexpand *macroexpand-hook*
    :setf-fdefinition sb-impl::*setf-fdefinition-hook*
    :setf-compiler-macro-function sb-int:*setf-compiler-macro-function-hook*
    :setf-macro-function sb-int:*setf-macro-function-hook*
    :default-dstate sb-disassem:*default-dstate-hooks*
    :debugger *debugger-hook*
    :debugger sb-ext:*invoke-debugger-hook*
    :executing-breakpoint sb-di::*executing-breakpoint-hooks*
    :defstruct sb-kernel::*defstruct-hooks*
    :define-condition sb-kernel::*define-condition-hooks*
    #+sb-fasteval :apply #+sb-fasteval sb-interpreter::*applyhook*
    #+sb-fasteval :self-apply #+sb-fasteval sb-interpreter::*self-applyhook*
    :stepper sb-ext:*stepper-hook*
    :save sb-ext:*save-hooks*
    :exit sb-ext:*exit-hooks*))

(defparameter *default-arena-size* (* 10 1024 1024 1024)
  "The default size of freshly allocated arenas.")

(defparameter *default-heap-size* (ash 1 16)
  "The default system heap size.")

(defun current-lisp-implementation ()
  "Return the current lisp implemenation as a list: (TYPE VERSION FEATURES)"
  (list 
   (lisp-implementation-type)
   (lisp-implementation-version)
   *features*
   *modules*))

(defun current-machine ()
  "Return the current machine spec as a list: (HOST TYPE VERSION)"
  (list
   (machine-instance)
   (machine-type)
   (machine-version)))

(defun list-package-symbols (&optional (pkg *package*))
  "List all external symbols of PKG."
  (loop for s being the external-symbol of pkg
        collect s))

(defun list-all-symbols ()
  "List all symbols found in this iamge."
  (let ((r)) 
    (dolist (p (list-all-packages) r) 
      (appendf r (list-package-symbols p)))))

(eval-always
  (defun package-symbols (&optional (package *package*) test)
    "List the symbols of PACKAGE which satisfy TEST if present."
    (let ((symbols))
      (do-external-symbols (symbol package)
        (if test
            (when (funcall test symbol)
              (push symbol symbols))
            (push symbol symbols)))
      symbols)))

(defun package-symbol-names (&optional (package *package*) test)
  "List the symbol names of PACKAGE which satisfy test if present."
  (sort (mapcar (lambda (x) (string-downcase (symbol-name x)))
                (package-symbols package test))
        #'string<))

(defmacro do-internal-symbols ((var package) &body forms)
  "Bind VAR to each internal symbol of PACKAGE in turn and evaluating FORMS for each."
  (std/sym:with-gensyms (state)
    `(do-symbols (,var ,package)
       (multiple-value-bind (,var ,state)
	   (find-symbol (symbol-name ,var) ,package)
	 (when (eq ,state :internal)
	   ,@forms)))))

(defun standard-symbol-names (&optional test)
  "List the ANSI standard list of symbols which satisfy TEST if present."
  (package-symbol-names :common-lisp test))

(defun handle-serious-condition (condition)
  "Handle a fatal CONDITION. Depending on whether *INTERACTIVE* is set, enter
debug or die."
  (cond
    (*interactive*
     (invoke-debugger condition))
    (t
     (with-sane-io-syntax 
       (let ((out (make-synonym-stream '*error-output*)))
         (format  out "~&Fatal condition:~%~A~%" condition)
         (sb-debug:print-backtrace :stream out))
       (when condition
         (format t "~A" condition)
         (sb-ext:quit :unix-status 99))))))

(eval-always
  (defvar *core-image-revived-p* nil
    "Set to T when the current image has been revived.")
  (defvar *core-image-revive-hooks* nil
    "List of hooks to be evaluated when an image is revived.")
  (defvar *core-image-entry-point* nil
    "Entrypoint associated with this core image."))

(defun revive-image (&key (interactive *interactive*)
                          (hooks *core-image-revive-hooks*)
                          (entry-point *core-image-entry-point*)
                          (if-already-revived '(cerror "Revive image anyway")))
  "Like UIOP:RESTORE-IMAGE but without a prelude."
  (when *core-image-revived-p*
    (if if-already-revived
        (funcall if-already-revived "Image already ~:[being ~;~]revived"
                 (eq *core-image-revived-p* t))
        (return-from revive-image)))
  (handler-bind ((serious-condition #'handle-serious-condition))
    (setf *interactive* interactive)
    (setf *core-image-revive-hooks* hooks)
    (setf *core-image-revived-p* :in-progress)
    (dolist (f *core-image-revive-hooks*)
      (funcall f))
    (setf *core-image-revived-p* t)
    (let ((results (multiple-value-list
                    (if entry-point
                        (funcall entry-point)
                        t))))
      (if interactive
          (values-list results)
          (sb-ext:exit :code (if (first results) 0 1))))))

;; TODO
(defun save-lisp-tree-shake-and-die (path &rest args)
  "A naive tree-shaker for lisp."
  ;; https://gist.github.com/burtonsamograd/f08f561264ff94391300
  (loop repeat 10
        do (sb-ext:gc :full t))
  (apply #'sb-ext:save-lisp-and-die path args))

(defparameter *gc-logfile* #P"gc.log")

(defun enable-gc-logfile (&optional (file *gc-logfile*))
  "Enable the system *GC-LOGFILE*."
  (setf (sb-ext:gc-logfile) file))

(defun forget-shared-object (name)
  "Forget the shared object specified by NAME."
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

(defun save-lisp (name &key force save make package compression verbose version callable-exports executable (toplevel #'sb-impl::toplevel-init) forget save-runtime-options root-structures (purify t))
  "Process NAME and keyword arguments then pass options to the underlying build
system - eventually terminating on SAVE-LISP-AND-DIE."
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
  "Eval BODY with float traps disabled - sometimes necessary when working with
shared libraries."
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
  "Decompose a 32-bit integer N into 4 octets."
  (declare ((unsigned-byte 32) n))
  (list
   (ldb (byte 8 0) n)
   (ldb (byte 8 8) n)
   (ldb (byte 8 16) n)
   (ldb (byte 8 24) n)))

(macrolet ((%with-cpuid (n &body body) 
             `(multiple-value-bind (a b c d) (cpuid ,n) 
                ,@body)))
  (defun cpu-vendor ()
    "Return the vendor of the host CPU."
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
    "Return the brand of the host CPU."
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

(defparameter %little-endian nil
  "An internal flag which indicates the host is little-endian, in the event that
we can't determine endianness at compile-time.")

(defun little-endian-p ()
  "Return T if the current platform is little-endian else NIL."
  #+(or :x86 :x86-64 :little-endian) t
  #+(or :PPC :POWERPC :big-endian) nil
  #-(or :x86 :x86-64 :little-endian :ppc :powerpc :big-endian)
  %little-endian)

(defun 64-bit-p () 
  "Return T on a 64-bit platform else NIL."
  #+x86-64 t
  #+x86 nil)

(defun 32-bit-p () 
  "Return T on a 64-bit platform else NIL."
  #+x86 t
  #+x86-64 nil)

#+nil
(defun register-project-directory (path &optional (asdf t))
  "Add PATH to QL:*LOCAL-PROJECT-DIRECTORIES* and ASDF:*CENTRAL-REGISTRY* (as
long as ASDF is non-nil)."
  #+quicklisp (pushnew path ql:*local-project-directories*)
  (when asdf (pushnew path asdf:*central-registry*)))

;;;_. Time
(definline get-real-time-seconds ()
  "Call GET-INTERNAL-REAL-TIME and convert the result to seconds."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun %time-remaining (start timeout)
  "Check the current time to see if TIMEOUT seconds have elapsed since START."
  (- timeout (- (get-real-time-seconds) start)))

(defmacro! with-countdown (o!time &body body)
  "Eval BODY with an implicit timeout TIME."
  (with-gensyms (start)
    `(let ((,start (get-real-time-seconds)))
       (flet ((time-remaining () (std/sys::%time-remaining ,start ,g!time)))
         (declare (inline time-remaining))
         ,@body))))

;;;_. Logical Pathnames
(defun list-all-logical-host-names ()
  "Return a list of currently available logical hosts."
  (map 'list (lambda (x) (slot-value x 'sb-impl::name)) *logical-hosts*))

(defmacro define-logical-pathname (host path &rest translations)
  "Define a new LOGICAL-PATHNAME associated with HOST and defaulting to
PATH. TRANSLATIONS is a list of (MATCH TRANSLATION) pairs."
  (unless (null path)
    (setf translations 
	  (append `((,(format nil "~A" host) ,path)) translations)))
  `(setf (logical-pathname-translations ,host)
         ;; eval second element only
	 ',(mapcar (lambda (x)
                     (setf (cadr x) (eval (cadr x)))
                     x)
                   translations)))

(defun check-logical-host (host)
  "Check a single LOGICAL-HOST, making sure all directories exist and are
accessible."
  (mapc (lambda (x) (ensure-directories-exist x :verbose t))
        (mapcar 
         (lambda (y)
           (let ((z (second y)))
             (if (wild-pathname-p z)
                 (make-pathname :directory (remove :wild-inferiors (pathname-directory z) :from-end t :count 2))
                 z)))
         (logical-pathname-translations host))))

(defun check-logical-hosts (&optional (hosts *logical-hosts*))
  "Check each member of *LOGICAL-HOSTS*, ensuring all directories exist and are
accessible."
  (map nil #'check-logical-host hosts))

(define-logical-pathname "STASH" "/opt/stash/"
  ("**;*.*.*" "/opt/stash/**/*.*"))

(define-logical-pathname "USER" "~"
  ("ORG;**;*.*.*" "~/org/**/*.*")
  ("SRC;**;*.*.*" "~/src/**/*.*")
  ("STASH;**;*.*.*" "~/.stash/**/*.*")
  ("STORE;**;*.*.*" "~/.store/**/*.*")
  ("**;*.*.*" "~/**/*.*"))


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
   (translate-logical-pathname "STASH:CACHE;lisp;**;*.*.*"))
  ("TMP;**;*.*.*" "/tmp/**/*.*"))

;;;_. Hexdump
;; https://stackoverflow.com/questions/69974963/object-memory-layout-in-common-lisp#70019565
(defun hexdump-object (obj)
  "Try to hexdump an object, including immediate objects. All the
work is done by sb-vm:hexdump in the interesting cases."
  #-64-bit
  (error "not a 64-bit SBCL")
  (let* ((address/thing (sb-kernel:get-lisp-obj-address obj))
         (tags (ldb (byte 4 0) address/thing)))
    (format t "~&lowtags: ~12T~4,'0b~%" tags)
    (cond
      ((zerop (ldb (byte 1 0) tags))
       (format t "~&fixnum:~12T~16,'0x = ~S~%" address/thing obj))
      ((= (ldb (byte 2 0) tags) #b01)
       (format t "~&immediate:~12T~16,'0x = ~S~%" address/thing obj))
      ((= (ldb (byte 2 0) tags) #b11)   ;must be true
       (format t "~&~A:~12T~16,'0x : ~16,'0x~%"
               (case (ldb (byte 2 2) tags)
                 (#b00 "instance")
                 (#b01 "cons")
                 (#b10 "function")
                 (#b11 "other"))
               address/thing (dpb #b0000 (byte 4 0) address/thing))
       ;; this tells you at least something (and really annoyingly
       ;; does not pad addresses on the left)
       (sb-vm:hexdump obj))
      ;; can't happen
      (t (error "mutant"))))
  (values))

(defun lisp-object-info (obj)
  "Take an object and return a list of type and allocation information."
  (std:hexdump-object obj)
  (list 
   :type (type-of obj)
   :type-class (std/type:type-class-name-of obj)
   :type-id (std/type:type-id obj)
   :prim-type (primitive-type-name-of obj)
   :alloc (sb-ext:heap-allocated-p obj)))

;;;_. FASLs
(definline check-fasl-file-header (path)
  "Return the fasl-header of file at PATH."
  (with-open-file (f path :element-type 'unsigned-byte) (sb-fasl::check-fasl-header f)))

(defun fasl-file-header= (a b)
  "Return T if A and B are paths to fasl files with equivalent headers."
  (equal (check-fasl-file-header a) (check-fasl-file-header b)))

;;;_. Tags
(eval-always
  (defun %sbcl-tagp (sfx)
    (lambda (x) 
      (let* ((s (string x))
             (l (length s)))
        (and (> l (length sfx))
             (equal sfx (subseq s (- l (length sfx)) l))
             (eql (vboundp! x) :constant))))))

(defconstant-eqx +widetags+
    (coerce 
     (sort
      (package-symbols 
       "SB-VM" 
       (%sbcl-tagp "-WIDETAG"))
      (lambda (x y) (< (symbol-value x) (symbol-value y))))
      'vector)
  (lambda (x y) (every 'eql x y)))

(defconstant-eqx +lowtags+
    (coerce
     (sort
      (package-symbols 
       "SB-VM" 
       (%sbcl-tagp "-LOWTAG"))
      (lambda (x y) (< (symbol-value x) (symbol-value y))))
     'vector)
  (lambda (x y) (every 'eql x y)))
