;;; defsys.lisp --- defsystem extension macros

;; The Core System Definition facility.

;;; Commentary:

;; replacement/wrapper for ASDF

;; goals:
;; - dynamic asdf compatibility
;; - replace quicklisp (will need to be in skel)
;; - share resources between system and dependency manager
;; - integrate with skel/packy (package distributor)
;; - async ready
;; - parallel compilation
;; - LISP ONLY -- multi-lang systems are handled by skel
;; notes:

;; operations we care about:
;; - read
;; - load
;; - compile

;; The =std/defsys= package is the last package defined in the =std=
;; system, it provides a DEFSYS macro which is our equivalent to the more
;; popular =ASDF:DEFSYSTEM=.

;; Once the =std/defsys= package is loaded the /general preference/
;; across the =core= is to use =STD/DEFSYS= (nickname is =SYS=) instead
;; of =ASDF=, although both are generally supported.

;; Defsys is designed in such a way as to be directly integrated with the
;; host Lisp compiler. System definitions (=SYS:DEFSYS= forms) may be
;; compiled to a FASL just like any other lisp file. Typically the file
;; type is changed to =".fsys"= to distinguish them from standard lisp
;; fasls - see the =SYS:COMPILE-SYS= function for details.

;;;; ASDF Compatibility

;; all =SYS:COMPONENT= classes have relevant =CHANGE-CLASS= methods
;; defined for their relevant ASDF counterparts so that you can translate
;; between the two when needed, but in general this is not needed.

;; Many SYS methods accept an ASDF keyword which defaults to the value of
;; =*ASDF-COMPATIBILITY*=.

;;; Code:
(in-package :std/defsys)
(declaim (optimize (speed 3)))
(in-readtable :std)

;;; Variables
(declaim (list *sysdefs*))
(defvar *sysdefs* nil
  "A list of files containing DEFSYS forms.")

(defvar-unbound *system-cache-directory*
    "Cached system data directory.")

(defvar-unbound *system-data-directory*
    "Persistent system data directory.")

(defun system-cache-dir (dir)
  (merge-pathnames dir *system-cache-directory*))
(defun system-data-dir (dir)
  (merge-pathnames dir *system-data-directory*))

(defvar *component-class-table* (make-hash-table))
(defvar *test-system* :rt)
(defvar *system-table* (make-hash-table :test 'equal)
  "An EQL hash-table containing NAME:SYSTEM pairs.")

(defvar *system-session-async-p* nil
  "When non-nil enable the :system-session thread-pool.")

(defvar *provider-table* (make-hash-table)
  "A hash-table containing PROVIDER functions.")

(defvar *defsys* nil
  "When non-nil, indicates the name of the system currently being defined (at
macro-expansion time) or the SYSTEM object itself. This variable is rebound
inside every DEFSYS form.")

(defvar *asdf-compatibility* nil
  "When non-nil, enable compatibility between STD/DEFSYS and SYSTEM - component
operations will use ASDF and DEFSYS will first pass all argument to
ASDF:DEFSYSTEM.")

(define-constant +sys-extension+ "sys" 
  :test 'string=
  :documentation "The default file extension used in system definitions.")

(defvar *module* nil 
  "The name of the current module.
This value is set when INIT is called on a SYSTEM instance and whenever
LOAD-MODULE is called.")

(defvar *module-table* (make-hash-table :test 'equal)
  "A table which maps modules names to objects.")

;;; Conditions
(define-condition system-condition () ())
(define-condition system-error (error system-condition) ())
(define-condition system-warning (warning system-condition) ())
(defwarning simple-system-warning (simple-warning system-warning) () (:reporter t))
(deferror simple-system-error (simple-error system-condition) () (:reporter t))

(deferror system-session-missing (system-error) ()
  (:report "Missing *SYSTEM-SESSION*.")
  (:reporter t))

(deferror defsys-load-error (system-error file-error)
  ((name :initarg :name :accessor error-name))
  (:report (lambda (c s) 
             (format s "System ~A not found after loading file ~A" 
                     (error-name c) (file-error-pathname c)))))

(defconstant +system-interrupt-tag+ 'system-interrupt-tag)

(defmacro with-system-restarts (&body body)
  `(catch +system-interrupt-tag+
     (restart-case (progn ,@body)
       (retry ()
         :report (lambda (s)
                   (format s "~@<Retry system method.~@:>"))))))

;; (retry)
;; (reset-session)
;; (init :sys)

;;; Sysdef Utils
;; system definitions are files ending with +SYS-EXTENSION+ containing lisp
;; code.
(defun fsys-cache-file (path)
  (merge-pathnames (make-pathname :type "fsys" :defaults path) *user-fasl-cache*))

(defun sysdefs (&optional (dir *default-pathname-defaults*) (recurse t))
  "Return a list of system definition pathnames found in DIR."
  (declare (ftype (sfunction (&optional pathname boolean) list)))
  (collecting
    (walk-directory dir 
      (constantly t)
      (constantly recurse)
      (lambda (x) 
        (mapc
         #'collect
         (directory-files x #.(format nil "*.~A" +sys-extension+)))))))

(defun sysdef (&optional (dir *default-pathname-defaults*))
  "Return the 'default' system definition path of the current directory, if it exists."
  (declare (ftype (sfunction (&optional pathname) t)))
  (when-let ((defs (sysdefs dir nil)))
    (if (= 1 (length defs))
        (car defs)
        (find (car (last (pathname-directory (the pathname dir)))) defs 
              :test 'string-equal
              :key (lambda (x) (pathname-name (the pathname x)))))))

(defun list-all-systems ()
  (std/hash:hash-table-values *system-table*))

(defun list-all-modules ()
  (std/hash:hash-table-alist *module-table*))

(defun list-all-providers ()
  (std/hash:hash-table-alist *provider-table*))

;;; Components
(defclass component ()
  ((name :initarg :name :accessor name)
   (path :initarg :path :accessor path)
   (require :initform nil :initarg :require :accessor component-require)))

(eval-always
  (defun register-component-class (name class)
    (unless (memq class #1=(gethash name *component-class-table*))
      (setf (gethash name *component-class-table*)
            (nconc #1# (ensure-list class))))))

(defmacro defcomponent (name supers slots &rest opts)
  (declare (dynamic-extent opts) (list opts))
  (let ((kw (find :keyword opts :key 'car)))
    (setf opts (delete :keyword opts :key 'car))
    `(prog1 (defclass* ,name ,(or supers '(component)) ,slots ,@opts)
       (register-component-class ,(cadr kw) (find-class ',name)))))

(defmethod make-load-form ((self component) &optional env)
  (make-load-form-saving-slots self
    :slot-names (mapcar 'sb-mop:slot-definition-name (class-slots (class-of self)))
    :environment env))

(defmethod print-object ((self component) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (name self))))

(defcomponent file-component (component) 
  ((type :accessor component-type))
  (:keyword :file))

(defcomponent pkg-component (file-component) 
  ((internal-package :accessor internal-package :initform nil)
   (use :accessor component-use :initform *default-pkg-component-use*)
   (default-package :accessor default-package :initform nil)
   (readtable :accessor component-readtable)
   feature
   (export :accessor component-package-export))
  (:keyword :pkg)
  (:documentation "A FILE-COMPONENT which contains a collection of packages. The *PACKAGE* is
automatically set to an internal-only package based on the system name and
supplied keywords. The *DEFPKG-HOOK* is bound to a function which collects new
package-names defined with DEFPKG inside the specified file."))

(defmethod initialize-instance :after ((self pkg-component) &key internal-package default-package)
  (unless (packagep (default-package self))
    ;; TODO: move to load
    (when default-package (setq *default-package* default-package))
    (setf (internal-package self)
          (std/macs:ifret (when internal-package (find-package internal-package))
            (make-package (or internal-package (gensym (name self))) :use (slot-value self 'use))))))

(defcomponent mod-component (component) 
  ((components :accessor components))
  (:keyword :mod))

(defmethod component-type ((self mod-component))
  nil)

(defun mod-component-p (c)
  (typep c 'mod-component))

(defmethod print-object ((self mod-component) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A :components ~{~A~^ ~}" (name self) (path self) 
            (when (slot-boundp self 'components)
              (mapcar 'name (components self))))))

(defcomponent dir-component (mod-component) 
  ((include :accessor component-include :initform ".*[.]lisp")
   (exclude :accessor component-exclude :initform ".*[.]fasl"))
  (:documentation "A MOD-COMPONENT which matches regexp patterns against all files in a
directory recursively.")
  (:keyword :dir))

(defcomponent grovel-component (file-component) 
  ((package :accessor component-package))
  (:documentation "A FILE-COMPONENT which matches a SB-GROVEL constants file.")
  (:keyword :grovel))

(defun find-component (path &optional (self (find-system *module*)))
  "Find a component designated by PATH which is either an atom designating a
component name or a list indicating a sequence of module component names
ending with the target component name."
  (declare (component self))
  (if (atom path)
      (find path (the list (components self)) :test 'string-equal :key 'name)
      (let ((c self))
        (loop for p in path
              with parents = (list c)
              if (and (not p) (not (null parents))) ; go back one level
              do (setf c (pop parents))
              else do
                 (progn 
                   (setf c (find p (the list (components c)) :test 'string-equal :key 'name))
                   (when (mod-component-p c)
                     (push c parents)))
              finally (return c)))))

#+nil
(defun expand-component-paths (c)
  "Walk the components of C, expanding PATH slots along the way to
absolute pathnames. Shouldn't be needed if all system components exist when
LOAD-SYS is called."
  (with-optimization (:speed 3 :safety 0)
    (labels ((%expand (comp)
               (when (probe-file (path comp)) (setf (path comp) (probe-file (path comp))))
               (when (and (mod-component-p comp) (components comp))
                 (let ((*default-pathname-defaults* (path comp)))
                   (mapc #'%expand (components comp))))))
      (mapc (the (function (component) (values)) #'%expand) (components c)))))

(defun expand-component-requires (c)
  "Walk the components of C, expanding REQUIRE slots along the way to
objects of type COMPONENT."
  (with-optimization (:speed 3 :safety 0)
    (labels ((%expand (comp)
               (let ((ptr c))
                 (when (component-require comp)
                   (setf (component-require comp)
                         (mapcar (lambda (x)
                                   (etypecase x
                                     ((or string symbol list)
                                      (find-component x ptr))
                                     (component x)))
                                 (component-require comp))))
                 (when (and (mod-component-p comp) (components comp))
                   (setf ptr comp)
                   (mapc #'%expand (components comp)))
                 (values))))
      (declare (dynamic-extent (function %expand)))
      (mapc (the (function (t) (values)) #'%expand) (components c)))))

;;; Provider
(eval-always
  (defun register-provider (name function)
    (setf (gethash name *provider-table*) function)))

(defun find-provider (key)
  (gethash key *provider-table*))

;; TODO 2026-04-18: describe-module

(defmacro defprovider (key args &body body)
  "Define a provider function which processes forms where the car is (eql KEY)."
  `(register-provider ,key (lambda ,args ,@body)))

(defun call-provider (name form)
  (when-let ((x (the function (find-provider name))))
    (apply x form)))

(defun register-module (key name val &optional append)
  (multiple-value-bind (v f) (gethash name *module-table*)
    (if f
        (setf (gethash name *module-table*) ; v is a plist
              (let ((w (or (and (not append) (std/list:remove-from-plist v key))
                           v)))
                ;; TODO 2026-04-08: 
                (if append 
                    (if (getf w key)
                        (progn (pushnew val (getf w key) 
                                        :test (lambda (x y) 
                                                (cart-typecase (x y)
                                                  ((list list) (equalp x y))
                                                  ((t t) (string-equal (name x) (name y))))))
                               w)
                        (progn (setf (getf w key) (list val)) w))
                    (nconc w (list key val)))))
        (setf (gethash name *module-table*) (list key (list val))))))

(defprovider :asdf (root path &optional name)
  (unless (find-module root :asdf)
    (register-module 
     :asdf root 
     (compile-and-eval 
      `(asdf:load-asd 
        ,(probe-file (merge-pathnames path (path (find-system root))))
        :name ,(or name root))))))

(defprovider :alien (root name &rest args)
  (register-module :alien root (compile-and-eval `(std/alien:define-alien-loader ,name ,@args))))

(defprovider :readtable (root name)
  (register-module :readtable root (compile-and-eval `(std/named-readtables:find-readtable ,name))))

(defprovider :prelude (root name &rest args)
  (register-module 
   :prelude
   root
   (compile-and-eval `(pkg::%defpkg* ,root (list ,name ,@args)))
   t))

(defprovider :default-package (root name)
  (register-module
   :default-package
   root
   (find-package* name name)))

(defprovider :internal-package (root name)
  (register-module
   :internal-package
   root
   (find-package* name name)))

(defprovider :packages (root name)
  (register-module
   :packages
   root
   (find-package* name name)
   t))

(defprovider :pool (root name)
  (register-module :pool root name t))

(defprovider :printer (root name)
  (register-module :printer root name t)) ;; (compile-and-eval `(find-printer ,name))))

(defprovider :annotations (root name)
  (register-module :annotations root name t)) ;; (compile-and-eval `(annotations ,name))))

(defprovider :io (root name &rest args)
  (register-module :io root (cons name args) t))

(defprovider :proto (root name &rest args)
  (register-module :proto root (cons name args) t))

(defprovider :sys (root &rest args)
  (register-module
   :sys
   root
   (compile-and-eval 
    `(defsys ,@args :path ,(or (system-path root) *compile-file-truename* *load-truename*)))))

(defprovider :bin (root &rest args)
  (let* ((namep (oddp (length args)))
         (name (if namep (car args) args))
         (args (if namep (cdr args) args)))
    (register-module
     :bin root 
     (compile-and-eval
      `(lambda ()
         (std/core:save-lisp
          ,(if (pathnamep name) name
               `(std/core:stash-pathname ,(string-downcase name)))
          :executable t
          ,@args))))))

(defprovider :lib (root &rest args)
  (let* ((namep (oddp (length args)))
         (args (if namep (cdr args) args))
         (name (if namep (car args) root)))
    (register-module
     :lib root 
     (compile nil
              `(lambda ()
                 (save-lisp ,name
                            ,@args))))))

(defprovider :tests (name &rest args)
  (let ((req (getf args :require))
        (comp (getf args :components :null)))
    (remf args :require)
    (remf args :components)
    (unless (member name req :test 'string-equal)
      (push name req))
    (unless (member *test-system* req :test 'string-equal)
      (push *test-system* req))
    (let ((sys (compile-and-eval
                `(defsys ,(%test-system-name name) ,@args 
                   :require ,req :class 'test-system 
                   :components ,(if (eql comp :null) '((:file "tests")) comp)
                   :path ,(or (system-path name)
                              *compile-file-truename* 
                              *load-truename*)))))
      (register-module
       :tests
       name
       sys))))

(defprovider :bench (name &rest args)
  (register-module 
   :bench name 
   `(defsys ,(%bench-system-name name) 
      ,@args 
      :class 'bench-system 
      :path ,(or (system-path name) *compile-file-truename* *load-truename*))))

;;; Module
;; Unlike MOD-COMPONENT/DIR-COMPONENT, based on ASDF:MODULE which is merely a
;; container for other COMPONENTs, Lisp MODULEs in the Core support the ANSI
;; CL notion of Modules and are further extended.

;; Modules in the core are essentially a 1:N mapping from an arbitrary name
;; (string or symbol) to tagged lisp objects we call providers. Providers are
;; designated by a keyword (the tag) and are responsible for returning a form
;; which will be evaluated on a call to INIT.

;; The REQUIRE slot is a list of provider forms which indicate the
;; dependencies of the module. A call to INIT will parse each element
;; individually and attempt to load any dependencies indicated into the
;; current image.

;; Note that calling INIT on a SYSTEM or MODULE is not the same as loading it
;; - the idea is that INIT prepares the current image so that operations don't
;; need to concern themselves with checking for external
;; dependencies. Internal dependencies still need to be coordinated between
;; operations - that's what the system plan is for.
(defclass module ()
  ((name :initarg :name :accessor name)
   (hook :initarg :hook :type hook :accessor hook)
   (provide :initarg :provide :accessor module-provide)
   (require :initarg :require :accessor module-require))
  (:documentation "All Lisp Modules contain at least a NAME, HOOK, PROVIDE and REQUIRE slot."))

(defun find-module* (name)
  (or (gethash name *module-table*)
      (when-let ((sys (gethash name *system-table*)))
        (init sys)
        (gethash name *module-table*)))) ; no recurse

(defun find-submodule (kind name mod)
  "Find a submodule of type KIND specified by NAME in a module's plist MOD."
  (let ((k (getf mod kind)))
    (cond
      ((null name) k)
      ((listp k)
       ;; FIX 2026-03-29: use of ignore-errors
       (ignore-errors
        (or
         (find name k :test 'string-equal :key 'name)
         (assoc name k :test 'equalp)))))))

(defun find-submodules (name &optional kind)
  (let* ((parents)
         (providers)
         (match
             (collecting
               (labels ((%sub (k v kin) 
                          (if kin
                              (when-let ((y (find-submodule kin name v)))
                                (collect y) 
                                (push k parents)
                                (push kin providers))
                              (mapc (lambda (x) (%sub k v x)) (hash-table-keys  *provider-table*)))))
                 (maphash (lambda (k v) (%sub k v kind)) *module-table*)))))
    (nreversef parents)
    (nreversef providers)
    (if (> (length match) 1)
        (progn
          (warn "Multiple matches found for module ~A ~A" kind name)
          (values match parents providers))
        (values (car match) (car parents) (car providers)))))

(definline provider-name-coerce (name)
  ":PACKAGE -> :PACKAGES.
Other conveniences as needed."
  (if (eql name :package) :packages name))

(defun find-module (name &optional kind key)
  "Find the module specified by NAME which should be a system designator or NIL
to match all systems and optional KIND (a provider-designator) specified by KEY."
  (when name (setf name (keywordicate name)))
  (when kind (setf kind (provider-name-coerce kind)))
  (cond
    (name
     (if-let ((mod (find-module* name)))
       (if kind
           (find-submodule kind key mod)
           mod)
       (find-submodules name kind)))
    (t (find-submodules key kind))))

;; (SET-MODULE NAME nil) deletes a module.
(defun set-module (name val &optional kind key (append t))
  (cond 
    ((and kind (not append))
     (let* ((ret (find-module name))
            (k (getf ret kind)))
       (when k (remf ret kind))
       (cond
         (val (pushnew val k :test 'string= :key 'name)
              (setf (gethash name *module-table*) (nconc ret (list kind k))))
         (key (setf k (remove key k :test 'string= :key 'name))
              (setf (gethash name *module-table*) (nconc ret (list kind k))))
         (t (setf (gethash name *module-table*) ret)))))
    ((and kind val append) (register-module kind name val append))
    ((not (or kind key val append))
     (delete-module name))
    (t (setf (gethash name *module-table*) val))))

(definline delete-module (name) (remhash name *module-table*))

(defsetf find-module (name &optional kind key append) (val)
  `(set-module ,name ,val ,kind ,key ,append))

(defun init-module (mod)
  "Initialize module MOD, loading all implementation hooks."
  (with-slots (hook) mod
    (maphash 
     (lambda (k v) (when-let ((x (gethash k (hook-value hook)))) (std/list:appendf v x)))
     (hook-value std/core::*sbcl-hooks*))))

(defvar *protocol-keyword-imports*
  '(:methods :functions :types :variables 
    :constants :parameters :macros :conditions
    :restarts :accessors :predicates :classes
    :structs :declarations :globals))

(defun %load-proto (form &optional (system *module*))
  "Load a protocol module given its FORM."
  (handler-case
      (destructuring-bind (name . args) form
        (declare (ignore name))
        (let ((pkg (find-package (or (getf args :package) system))))
          (values
           (remove-if 'null
                      (mapcar (lambda (x)
                                (when-let ((syms (getf args x)))
                                  (shadowing-import (mapcar (lambda (x) (intern (symbol-name x) pkg)) syms))
                                  (cons x syms)))
                              *protocol-keyword-imports*))
           pkg)))
    (error (c) (simple-system-error "Invalid protocol.~%~A" c))))

;; templates?
(defun %load-module (form kind key sys)
  (if (and kind (not key) (consp form) (consp (car form)))
      (mapcar (lambda (x) (%load-module x kind key sys)) form)
      (case kind
        (:internal-package nil) ; ignore, never ensure
        (:default-package (eval-always (setq *package* form)))
        ;; should assert io and proto symbols are available, maybe set an *io* and *proto* variable.
        (:io (gethash form *io-table*))
        (:annotations (load-annotations (car form)))
        (:printer (use-printer form))
        (:alien (funcall (the function (gethash form std/alien:*alien-load-table*))))
        (:prelude (use-package (ensure-car form)))
        (:package (use-package (ensure-car form)))
        (:packages (mapc (lambda (x) (or (packagep x) (simple-system-error "Invalid package: ~A" x))) form))
        (:pool (setf *thread-pool* (find-thread-pool form)))
        (:proto (%load-proto form))
        (:tests (load-system form))
        (:sys (load-system form))
        (:bench (load-system form))
        (:readtable (apply 'std/named-readtables:merge-readtables-into *readtable* (ensure-cons form)))
        (:asdf (when *asdf-compatibility* (funcall form)))
        ('nil
         (sb-int:doplist (k v) form
           (%load-module v k nil nil)))
        (t (simple-system-error "Unknown provider: ~A" kind)))))

(defun load-module (name &optional kind key)
  (let ((form (find-module name kind key)))
    (typecase form
      (list
       (%load-module
        (case (length form)
          (1 (car form))
          (t form))
        kind (ensure-car form) *module*))
      (t (%load-module form kind key *module*)))
    form))

(defun load-module* (name &rest args)
  (mapcar (lambda (x) 
            (if (atom x)
                (load-module name x)
                (apply 'load-module name x)))
          args))

(defun load-modules (&rest args)
  (mapcar (lambda (x) 
            (if (atom x) 
                (load-module x)
                (apply 'load-module* x)))
          args))

(defun unload-module (name &optional kind key)
  (setf (find-module name kind key) nil)
  (when (eq *module* name)
    (setf *module* nil)))

(defun module-provide-system (name)
  "Provide a SYSTEM, adding valid entries to the *MODULES* variable. The function
USE should be called in order to load and activate a module."
  (when-let ((sys (find-system name)))
    (load-system sys)
    t))

(defmacro with-module (name &body body)
  "Load the module named NAME, binding it to *MODULE* and eval BODY."
  `(let ((*module* (or (require ,name) ,name)))
     ,@body))

(defmacro use (name &body body)
  "Load and activate a package or module by NAME with the provider forms in BODY."
  (if body
      `(load-module* ,name ,@body)
      (if (find-package name)
          `(use-package ,name)
          `(load-module ,name))))

(defmacro using (&rest args)
  `(progn
     ,@(mapcar (lambda (x) (if (atom x) `(use ,x) `(use ,@x))) args)))

(defmacro refuse (name &body body)
  "Unload and deactivate a package or module by NAME with arguments BODY."
  (if body
      `(unload-module ,name ,@body)
      (if (find-package name)
          `(unuse-package ,name)
          `(unload-module ,name))))

(defmacro refusing (&rest args)
  `(progn
     ,@(mapcar (lambda (x) (if (atom x) `(refuse ,x) `(refuse ,@x))) args)))

;; (defmacro with-eval-after-load (module &body body))

;;; System
(defcomponent system (mod-component module)
  ((version :accessor version :initform nil)
   (description :accessor description :initform nil)
   (plan 
    :documentation "The default plan associated with this object which specifies the ordering of
system jobs to be executed in an async context."
    :initform :serial
    :accessor plan))
  (:keyword :sys)
  (:default-initargs :hook (make-instance 'key-hook))
  (:documentation "Base class for system definitions found throughout the core (*.sys)."))

(defun system-equal (a b)
  "Return T if systems A and B refer to the same SYSTEM."
  (and (equal (name a) (name b))
       (equal (version a) (version b))
       (equal (path a) (path b))))

(defmethod add-hook ((self system) function &rest args)
  (apply 'add-hook (hook self) function args))

(defmethod print-object ((self system) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A~@[ ~A~]" (name self) (version self))))

(defun system-path (name)
  (declare (ftype (sfunction (t) pathname)))
  (if (typep name 'system) (path name) (path (find-system name :default :error))))

(definline system-home (name)
  (make-pathname :directory (pathname-directory (the pathname (system-path name)))))

(defun system-relative-pathname (self path)
  (merge-pathnames path (system-home self)))

(defun find-system-dependents (system)
  "Return a list of systems which depend on SYSTEM."
  (when (typep system 'system) (setf system (name system)))
  (let ((r))
    (dolist (s (list-all-systems) r)
      (when (and s (member (name system)
                           (mapcar
                            (lambda (dep)
                              (when (atom dep)
                                (string-downcase (format nil "~A" dep))))
                            (component-require s))
                           :test #'equalp))
        (push s r)))))

(defun directory-systems (&optional (dir *default-pathname-defaults*) include-tests)
  "Return a list of all systems under DIR. When INCLUDE-TESTS is non-nil also include TEST-SYSTEMs."
  (let ((dir (make-pathname :directory `(,@(pathname-directory (truename dir)) :wild-inferiors))))
    (remove-if
     (lambda (x) (or (and (not include-tests) (typep x 'test-system))
                     (not (pathname-match-p (path x) dir))))
     (list-all-systems))))

(defun directory-sysdef-files (&optional (dir *default-pathname-defaults*))
  "Return a list of all sysdef files under DIR."
  (let ((dir (make-pathname :directory `(,@(pathname-directory (truename dir)) :wild-inferiors))))
    (remove-duplicates
     (mapcan (lambda (x) (and (pathname-match-p (path x) dir) (list (path x)))) (list-all-systems))
     :test 'pathname-equal)))

;;; ASDF Compat
(definline change-component-class (self)
  "Change class of SELF to its associated STD/DEFSYS class."
  (etypecase self
    (asdf:system (change-class self 'system))
    (asdf:module (change-class self 'mod-component))
    (asdf:file-component (change-class self 'file-component))
    (asdf:component (change-class self 'component))))

(definline revert-component-class (self)
  "Revert std/defsys class SELF to its associated ASDF class."
  (etypecase self
    (system (change-class self 'asdf:system))
    (mod-component (change-class self 'asdf:module))
    (grovel-component (change-class self 'sb-grovel:grovel-constants-file))
    (file-component (change-class self 'asdf:file-component))
    (component (change-class self 'asdf:component))))

(defmethods change-class 
  ;; components
  (((instance asdf:component) (new-class-name (eql 'component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)))
  (((instance component) (new-class-name (eql 'asdf:component)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (asdf:component-pathname instance)))
  (((instance asdf:file-component) (new-class-name (eql 'file-component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :type (asdf:file-type instance)))
  (((instance file-component) (new-class-name (eql 'asdf:file-component)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (path instance)
     :type (component-type instance)))
  (((instance grovel-component) (new-class-name (eql 'sb-grovel:grovel-constants-file)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (path instance)
     :type (component-type instance)
     :package (component-package instance)))
  (((instance sb-grovel:grovel-constants-file) (new-class-name (eql 'grovel-component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :type (asdf:file-type instance)
     :package (component-package instance)))
  (((instance asdf:module) (new-class-name (eql 'mod-component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :components (mapcar 'change-component-class (asdf:component-children instance))))
  (((instance mod-component) (new-class-name (eql 'asdf:module)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (asdf:component-pathname instance)
     :components (mapcar 'revert-component-class (components instance))))
  ;; system
  (((instance asdf:system) (new-class-name (eql 'system)) &key)
   (make-instance new-class-name
     :version (asdf:component-version instance)
     :name (keywordicate (string-upcase (asdf:component-name instance)))
     :path (asdf:component-pathname instance)
     :description (asdf::component-description instance)
     :components (mapcar 'change-component-class (asdf:component-children instance))))
  (((instance system) (new-class-name (eql 'asdf:system)) &key)
   (warn 'simple-system-warning 
         :format-control "Erasing system slots (:require :provide :hook) from system ~A." 
         :format-arguments (name instance))
   (make-instance new-class-name
     :version (version instance)
     :name (name instance)
     :description (description instance)
     :components (mapcar 'revert-component-class (components instance)))))

;;; Test System
(defcomponent test-system (system) ()
  (:keyword :tests))

(defmethod print-object ((self test-system) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A~@[ ~A~]" (name self) (version self))))

(defun test-system-name-p (name)
  (std/seq:ends-with-subseq "/TESTS" (string-upcase name)))

(definline list-all-test-systems ()
  "Return a list of all TEST-SYSTEMs."
  (collecting (maphash (lambda (k v) (when (test-system-name-p k) (collect v))) *system-table*)))

(definline %test-system-name (name)
  (concatenate 'simple-base-string (string-upcase name) "/TESTS"))

(defmethod change-class ((instance asdf:system) (new-class-name (eql 'test-system)) &key)
  (make-instance new-class-name
    :version (asdf:component-version instance)
    :name (keywordicate (string-upcase (asdf:component-name instance)))
    :path (asdf:component-pathname instance)
    :description (asdf::component-description instance)
    :components (mapcar 'change-component-class (asdf:component-children instance))))

;;;; Bench System
(defcomponent bench-system (system) ()
  (:keyword :bench))

(defmethod change-class ((instance asdf:system) (new-class-name (eql 'bench-system)) &key)
  (make-instance new-class-name
    :version (asdf:component-version instance)
    :name (keywordicate (string-upcase (asdf:component-name instance)))
    :path (asdf:component-pathname instance)
    :description (asdf::component-description instance)
    :components (mapcar 'change-component-class (asdf:component-children instance))))

(defun bench-system-name-p (name)
  (std/seq:ends-with-subseq "/BENCH" (string-upcase name)))

(definline %bench-system-name (name)
  (concatenate 'simple-base-string (string-upcase name) "/BENCH"))

;; preloaded
;; immutable

;;; Session
(eval-always
  (defvar *system-session-capacity* 256
    "The maximum count of systems which are allowed to wait in the system queue for processing.")
  (defvar *system-task-capacity* 32 "The maximum count of system-tasks which are allowed to wait in the task queue
for processing.")
  (defstruct system-session
    "A reusable session in which SYSTEMs may be processed."
    ;; A simple cache of TASK results
    (task-cache (make-hash-table))
    ;; A simple cache of file operation times (:read :write :load :compile)
    (file-cache (make-hash-table :test 'equal))
    ;; A thread-pool which is dedicated to running system tasks
    pool
    ;; A queue of system tasks.
    (tasks (make-queue :capacity *system-task-capacity* :element-type 'std/task::task))
    (states (make-queue :capacity *system-task-capacity* :element-type 'std/task::status))))

(defvar *system-kernel* (make-kernel #'std/thread::%work))

(defclass system-worker (task-worker) ()
  (:documentation "A worker created in a SYSTEM-SESSION pool.")
  (:default-initargs 
   :name "sys"
   :kernel *system-kernel*
   :bind (append *default-special-bindings* '((*kernel* *system-kernel*)))))

(defmethod start ((self system-session))
  (start (system-session-pool self)))

(defmethod stop ((self system-session) &key)
  (stop (system-session-pool self)))

(defmethod tasks ((self system-session))
  (system-session-tasks self))

(sb-ext:defglobal *system-session* (make-system-session)
    "Global SYSTEM-SESSION or NIL when no systems have been initialized.")

(defmethod reset ((self system-session) &key)
  (setf *system-session* (make-system-session)))

(defun check-system-session ()
  (assert *system-session* (*system-session*) 'system-session-missing))

(defmacro with-system-session ((&optional sym sys) &body body)
  "Bind *SYSTEM-SESSION* to SYM around BODY. WHEN SYS is non-nil it is expected
to be a system which is pushed to the session queue before BODY."
  (if sym
      (multiple-value-bind (%body %decl) (std/prim:parse-body body)
        `(with-system-restarts
           (let ((,sym *system-session*)
                 ,@(when sys `((*default-pathname-defaults* 
                                (if (pathnamep ,sys)
                                    ,sys
                                    (pathname (directory-namestring (probe-file (path ,sys)))))))))
             ,@%decl
             ,@%body)))
      `(with-system-restarts (progn ,@body))))

(eval-always
  (defun cached-system-file (f)
    (gethash f (system-session-file-cache *system-session*)))
  (defun (setf cached-system-file) (new f)
    (setf (gethash f (system-session-file-cache *system-session*)) new))
  (defun update-cached-system-file (f &optional load compile)
    (setf (cached-system-file f)
          `(:read ,(get-universal-time)
            :write ,(file-write-date f)
            ,@(when load `(:load ,(the fixnum (get-universal-time))))
            ,@(when compile `(:compile ,(the fixnum (get-universal-time))))))))

(defmacro with-system-file ((file &key load compile) &body body)
  `(progn 
     (ensure-system-file-cached ,file)
     ,@body
     (update-cached-system-file ,file ,load ,compile)))

;; TODO 2026-03-03: 
;; (defmacro component-case ())

(defun reload-p (file)
  "Given a FILE, return T if it needs to be reloaded."
  (if-let ((f (cached-system-file file)))
    (lety ((w (setf (getf f :write) (file-write-date file)) :type fixnum)
           (l (or (getf f :load) 0) :type fixnum))
      (> w l))
    t))

(defun recompile-p (file)
  "Given a FILE, return T if it needs to be recompiled."
  (if-let ((f (cached-system-file file)))
    (lety ((c (or (getf f :compile) 0) :type fixnum)
           (w (setf (getf f :write) (file-write-date file)) :type fixnum))
      (> w c))
    t))

(defun component-reload-p (comp)
  "Return T when component COMP should be reloaded."
  (declare (component comp))
  (if (mod-component-p comp)
      (some #'reload-p (components comp))
      (reload-p (path comp))))

(defun component-recompile-p (comp)
  "Return T when component COMP should be recompiled."
  (declare (component comp))
  (if (mod-component-p comp)
      (some #'recompile-p (components comp))
      (recompile-p (path comp))))

(defun ensure-system-file-cached (file)
  (unless (cached-system-file file) 
    (setf (cached-system-file file)
          `(:read 0 :write ,(file-write-date file) :load 0 :compile 0))))           

;;; Tasks
;; System Tasks are simple function which take a single component as an argument
(defkernel system-task (task)
  ((name :reader name :initarg :name :initform (gensym "SYSTEM-TASK"))))

(defmethod initialize-instance :after ((self system-task) &key)
  ;; (check-system-session)
  (setf (gethash (name self) (system-session-task-cache *system-session*)) self))

(defun make-system-task (thunk &key (name (gensym "SYS-TASK")) (state (std/task::make-status)))
  (let ((task (make-instance 'system-task :name name :state state)))
    (setf (kernel task) thunk)
    task))

(defmacro with-system-task ((sym fn &rest args) &body body)
  "Create and return a new SYSTEM-TASK which is pushed to the task queue after executing BODY."
  `(let ((,sym (make-system-task ,fn ,@args)))
     (unwind-protect (progn ,@body)
       (push-queue ,sym (system-session-tasks *system-session*))
       (push-queue (state ,sym) (system-session-states *system-session*)))))

(defgeneric needed-in-image-p (task component)
  (:documentation "Is the action of TASK on COMPONENT needed in the current image
to be meaningful, or could it just as well have been done in another Lisp
image?"))

(defmethod needed-in-image-p ((o task) (c component))
  ;; We presume that actions that modify the filesystem don't need be run
  ;; in the current image if they have already been done in another,
  ;; and can be run in another process (e.g. a fork),
  ;; whereas those that don't are meant to side-effect the current image and can't.
  (not (output-files o c)))

(defgeneric output-files (task component)
  (:documentation "Methods for this function return two values: a list of output files
corresponding to this action, and a boolean indicating if they have already been subjected
to relevant output translations and should not be further translated.

Methods on PERFORM *must* call this function to determine where their outputs
are to be located. They may rely on the order of the files to discriminate
between outputs."))

(defgeneric input-files (operation component)
  (:documentation "A list of input files corresponding to this action.

Methods on PERFORM *must* call this function to determine where their inputs
are located. They may rely on the order of the files to discriminate between
inputs."))

;; mark-component-preloaded

;;; Jobs
;; System Jobs are effectively plans composed of system tasks
(defkernel system-job (job system-task) ())

(defmacro with-system-job ((sym &rest args) &body body)
  `(let ((,sym (make-instance 'system-job ,@args)))
     . ,body))

;;; Defsys
(defun %parse-provide-form (form)
  (let ((ret))
    (mapc
     (lambda (x)
       (if (atom x) ; add to *MODULES*
           (pushnew (string-upcase x) *modules* :test 'string-equal)
           (push x ret)))
     form)
    (nreverse ret)))

(defun %parse-require-form (form)
  (mapc
   (lambda (x)
     (let ((y (if (atom x) x (car x))))
       (if-let ((z (or (find-system y) 
                       (ensure-car (apply 'find-module (mapcar 'keywordicate (split-sequence #\/ (string-upcase x))))))))
         (init z)
         (simple-system-error "System not found: ~A" y))))
   form)
  form)

(defvar *wildcard-regexp* (ppcre:create-scanner ".*"))

(defun %mod-component-walk (c &optional inc exc)
  (walk-directory (path c)
    (constantly t) (constantly t)
    (lambda (x)
      (dolist (f (directory-files x))
        (let ((f (namestring f))) ; set name only
          (when (and inc (ppcre:scan inc f) (or (not exc) (not (ppcre:scan exc f))))
            (push (%parse-component-form f) (components c)))))))
  ;; fill in the path
  (mapc (lambda (x) 
          (setf (path x) 
                (probe-file (make-pathname 
                             :name (name x) 
                             :type (when-let ((ctyp (component-type x))) (string-downcase ctyp))
                             :directory (namestring (path c))))))
        (components c))
  c)

(defun %parse-component-form (form)
  (if (atom form) ; atoms will populate a NAME and TYPE but not a PATH
      (lety ((dir (make-pathname :defaults form) :type pathname))
        (if (directory-path-p dir)
            (make-instance 'dir-component
              :name (last (pathname-directory dir)))
            (make-instance 'file-component 
              :type (or (pathname-type dir) "lisp")
              :name (pathname-name dir))))
      (let ((n (cadr form))
            (kind (car (gethash (car form) *component-class-table*)))
            (props (cddr form)))
        (ecase (car form)
          ((or :file :pkg :grovel)
           (let ((ty (if (pathnamep n) (pathname-type n) "lisp")))
             (when (symbolp n) (setf n (string-downcase n)))
             (apply 'make-instance kind
                    :type (keywordicate (string-upcase ty))
                    :name n
                    :path (probe-file (make-pathname :name n :type ty :defaults *default-pathname-defaults*))
                    props)))
          (:mod
           (let* ((%path (make-pathname :name n :defaults *default-pathname-defaults*))
                  (path (or (std/file:probe-directory %path)
                            (simple-system-error "Component path not found: ~A" %path)))
                  (*default-pathname-defaults* path))
             (%mod-component-walk
              (make-instance kind
                :name n 
                :path path
                :components 
                (mapcar '%parse-component-form (getf props :components))
                :require (getf props :require)))))
          (:dir
           (let* ((path (std/file:probe-directory (make-pathname :name n :defaults *default-pathname-defaults*)))
                  (inc (or (getf props :include) *wildcard-regexp*))
                  (exc (getf props :exclude))
                  (c (make-instance kind
                       :name n
                       :path path
                       :include inc
                       :exclude exc
                       :components (mapcar '%parse-component-form (getf props :components)))))
             (%mod-component-walk c inc exc)))))))

(defun %parse-components-form (form)
  (let ((*default-pathname-defaults* 
          (or (when *defsys* 
                (make-pathname :directory (pathname-directory (the pathname (system-path *defsys*)))))
              *default-pathname-defaults*)))
    (mapcar '%parse-component-form form)))

(defmacro %make-sys (name class &body args)
  `(progn
     (if *asdf-compatibility*
         (change-class (defsystem ,name . ,args) ,class)
         ;; args are ignored if *ASDF-COMPATIBILITY* is nil. Make sure to fill
         ;; in the slots of the return value.
         (make-instance ,class :name ,name . ,args))))

(defun %sys-get (n body)
  (when-let ((v (getf body n)))
    (remf body n)
    v))

;; (defun expand-module-provides (comp)
;;   "Walk the PROVIDE slot of COMP, expanding provider results."
;;   (with-optimization (:speed 3 :safety 0)
;;     (when (module-provide comp)
;;       (loop for (k . v) in (module-provide comp)
;;             do (case k
;;                  (:sys (setf (std/list:assoc-value (module-provide comp) k) `(defsys ,@v))))))))

(defmacro defsys (name &body body)
  "Define a SYSTEM with NAME and BODY interpreted similar to ASDF:DEFSYSTEM.

SYSTEM objects register their own ASDF:SYSTEM objects as needed and provide
the following extensions:
- :HOOK       hook specs
- :PROVIDE    system-provided features, modules, readtables
- :REQUIRE    system/component required modules, features, and components"
  (multiple-value-bind (%body dec doc) (std/prim:parse-body body :documentation t)
    (declare (ignore dec))
    (unless (symbolp name) (setq name (keywordicate (string-upcase name))))
    (let ((prov (%sys-get :provide %body)) (hooks (%sys-get :hook %body))
                                           (path (%sys-get :path %body))
                                           (req (%sys-get :require %body))
                                           (ver (%sys-get :version %body))
                                           (plan (or (%sys-get :plan %body) :serial))
                                           (class (or (%sys-get :class %body) ''system))
                                           (comp (%sys-get :components %body))
                                           (*defsys* name))
      (std/sym:with-gensyms (sys)
        `(let* ((,sys (apply 'make-instance ,class :name ,name ',%body))
                (*default-pathname-defaults*
                  (if-let ((fpath (or *compile-file-truename* *load-truename* ,path)))
                    (make-pathname :directory (pathname-directory fpath))
                    *default-pathname-defaults*))
                (*defsys* ,sys))
           (setf (path ,sys) (or ,path *compile-file-truename* *load-truename*)
                 (slot-value ,sys 'plan) ,plan
                 (slot-value ,sys 'description) ,doc
                 (slot-value ,sys 'version) ,ver
                 (slot-value ,sys 'components) (%parse-components-form ',comp)
                 (slot-value ,sys 'provide) (%parse-provide-form ',prov)
                 (slot-value ,sys 'require) ',req)
           (mapc (lambda (x) (add-hook 
                              (hook ,sys) 
                              (if (or (symbolp (cadr x)) (functionp (cadr x)))
                                  x
                                  (list (car x) 
                                        (compile (gensym (string (car x)))
                                                 `(lambda () ,@(cdr x)))))))
                 ',hooks)
           (expand-component-requires ,sys)
           (setf (gethash ,name *system-table*) ,sys)
           ,sys)))))

(defun compile-sys (path &optional force output-file)
  "Compile a system's defsys file by PATH. Default extension is FSYS."
  (unless (pathnamep path) (setf path (or (when-let ((sys (find-system path))) (path sys)) (probe-file path))))
  (when (or (recompile-p path) force)
    (checked-compile-file path
                          :output-file (or output-file (ensure-fasl-cache-file path "fsys"))
                          :entry-points '(load-sys))
    (update-cached-system-file path nil t)))

(defun load-sys (path &optional name)
  "Load a SYS file from PATH. Unlike LOAD-ASD this function calls LOAD
internally. On success the path is added to the *SYSDEFS* list."
  (lety ((path 
           (etypecase path
             ((or string pathname) (truename path))
             (symbol (find (symbol-name path) *sysdefs* :key (lambda (x) (pathname-name (the pathname x)))
                                                        :test 'string-equal)))
           :type pathname))
    (with-system-session (s path)
      (declare (ignore s))
      (let ((%path (or (and (not (recompile-p path)) (probe-file (fasl-cache-file path "fsys"))) path)))
        (when 
            (restart-case (load %path)
              (load-file (p)
                :report "Load a different file." 
                :interactive (lambda () 
                               (list (setf %path (interact-line "File: "))))
                (load p)))
          (update-cached-system-file path t)
          (pushnew path *sysdefs* :test 'equal)
          (if name 
              (find-system name :default (lambda () (error 'defsys-load-error :name name :pathname path)))
              t))))))

;;; Templates
;; (define-template-generic (s.load #'subtypep) sym (&key))
;; (define-template-generic (s.compile #'subtypep) sym (&key))
;; (define-template-generic (s.save #'subtypep) sym (&key))
;; (define-template-generic (c.load #'subtypep) sym (&key))
;; (define-template-generic (c.compile #'subtypep) sym (&key))
;; (define-template-generic (c.save #'subtypep) sym (&key))

;; (defmacro define-system-method ())
;; (defmacro define-component-method ())
(defun read-component (comp &key (external-format :default) (package *package*))
  "Read a component from its PATH slot."
  (declare (ftype (sfunction (component &key (external-format t)) component)))
  (etypecase comp
    (mod-component (mapcar 'read-component (components comp)))
    ((or grovel-component pkg-component)
     (with-safe-io-syntax ((or (when (slot-boundp comp 'std/defsys::default-package) (slot-value comp 'std/defsys::default-package))
                               *package*) 
                           ;; NOTE: *read-eval* = T
                           t)
       (read-lisp-file (path comp) :external-format external-format)))
    (component (with-safe-io-syntax (package t)
                 (read-lisp-file (path comp) :external-format external-format)))
    ((or string pathname) (with-safe-io-syntax (package t) (read-lisp-file comp :external-format external-format)))))

(defun compile-grovel-component (comp)
  "Compile a GROVEL-COMPONENT."
  (lety* ((path (path comp) :type pathname)
          (output (fasl-cache-file path))
          (tmp-c-source (merge-pathnames #p"foo.c" output))
          (tmp-a-dot-out (merge-pathnames #p"a.out"
                                          output))
          (tmp-constants (merge-pathnames #p"constants.lisp-temp"
                                          output)))
    (sb-grovel::c-constants-extract path tmp-c-source (package-name (slot-value comp 'package)))
    (lety ((code (sb-grovel::run-c-compiler tmp-c-source tmp-a-dot-out) :type fixnum))
      (unless (= code 0)
        (error 'sb-grovel::c-compile-failed)))
    (lety ((code (sb-ext:process-exit-code
                  (sb-ext:run-program (namestring tmp-a-dot-out)
                                      (list (namestring tmp-constants))
                                      :search nil
                                      :input nil
                                      :output *trace-output*))
                 :type fixnum))
      (unless (= code 0)
        (error 'sb-grovel::a-dot-out-failed)))
    (multiple-value-bind (out warnings-p failure-p)
        (checked-compile-file tmp-constants :output-file output)
      (std/comp:check-lisp-compile-results out warnings-p failure-p))))

(defun compile-pkg-component (comp)
  "Compile a PKG-COMPONENT."
  (let ((output (ensure-fasl-cache-file (path comp))))
    (multiple-value-bind (out warnings-p failure-p)
        (checked-compile-file (path comp) :output-file output :verbose *verbose*)
      (std/comp:check-lisp-compile-results out warnings-p failure-p))))

(defun compile-component (comp &key (verbose *verbose*) force)
  "Compile a component."
  (declare (ftype (sfunction (component &key (verbose boolean) (force boolean)) component)))
  (when (or (component-recompile-p comp) force)
    (etypecase comp
      (mod-component (mapcar (lambda (x) (compile-component x :verbose verbose :force force)) (components comp)))
      (grovel-component (with-system-file ((path comp) :compile t) (compile-grovel-component comp)))
      (pkg-component (with-system-file ((path comp) :compile t) (compile-pkg-component comp)))
      (file-component
       (let ((f (path comp)))
         (when (or (recompile-p f) force)
           (with-system-file (f :compile t) 
             (checked-compile-file f :output-file (ensure-fasl-cache-file f) :verbose verbose)))))))
  comp)

(defun load-component-file (comp &key force verbose)
  (when force (compile-component comp :verbose verbose :force t))
  (load-component comp :force force :verbose verbose))

(defun load-component (comp &key force (verbose *verbose*))
  "Load a component."
  (declare (ftype (sfunction (component &key (force boolean)) component)))
  (when (or (component-reload-p comp) force)
    (etypecase comp
      (mod-component 
       (let ((*component-packages* nil))
         (mapcar (lambda (x) (load-component-file x :force force :verbose verbose))
                 (components comp))))
      (file-component
       (let ((f (path comp)))
         (when (or (reload-p f) force)
           ;; TODO: be smarter about which file to load
           (with-system-file (f :load t)
             (typecase comp
               (grovel-component 
                (compile-grovel-component comp) 
                (load (resolve-fasl-cache-file f) :verbose verbose))
               (pkg-component
                (let ((*package* (internal-package comp))
                      (pkg:*defpkg-hook* (lambda (x) (pushnew (package-name x) pkg:*component-packages* :test 'string=))))
                  (when-let ((f (and (slot-boundp comp 'feature) (slot-value comp 'feature)))) (pushnew f *features*))
                  (when-let ((r (and (slot-boundp comp 'readtable) (slot-value comp 'readtable))))
                    (setf *readtable* (std/named-readtables:ensure-readtable r)))
                  (compile-component comp :verbose verbose :force force)
                  (prog1 (load (resolve-fasl-cache-file f) :verbose verbose)
                    (when-let ((e (and (slot-boundp comp 'export) (slot-value comp 'export))))
                      (unless (find-package e) (make-package e :internal-symbols 0))
                      (reexport-packages *component-packages* e))
                    (mapc (lambda (x) (call-provider :packages (list *module* x))) *component-packages*)
                    (call-provider :internal-package (list *module* (internal-package comp)))
                    (call-provider :default-package (list *module* (or (default-package comp)
                                                                       (find-package* *module* nil)
                                                                       (car *component-packages*))))
                    (setq pkg:*component-packages* nil
                          pkg:*defpkg-hook* nil))))
               (t (compile-and-load f :output-file (ensure-fasl-cache-file f)
                                      :verbose verbose)))))))))
  comp)

(defun make-system-session-pool (&optional (thread-count (std/alien:num-cpus)))
  (make-thread-pool thread-count
                    :name :system-session
                    :class 'std/task:task-pool
                    :worker-class 'system-worker))

;;; Protocol
(defmethod init ((self (eql :sys)) &key (sysdefs (sysdefs)) (preload t) (pool *system-session-async-p*) (reset t)
                                        fasl-cache
                                        system-data
                                        system-cache)
  "Initialize STD/DEFSYS variables given a list of system directories SYSDEFS and
optionally calling LOAD-SYS on them when PRELOAD is T (default)."
  (init :xdg)
  (when sysdefs (setq *sysdefs* sysdefs))
  (setf *user-fasl-cache* (ensure-directories-exist (or fasl-cache (std/os:user-fasl-cache)))
        *system-data-directory* (or system-data (xdg-data-directory "lisp/sys"))
        *system-cache-directory* (ensure-directories-exist (or system-cache (xdg-cache-directory "lisp/sys")))
        (std/core:logical-pathname-translation "SYS" "CACHE;**;*.*.*") (namestring (merge-pathnames "**/*.*" *user-fasl-cache*)))
  (ensure-directories-exist (system-data-dir "bin/"))
  (pushnew 'std/defsys::module-provide-system sb-ext:*module-provider-functions*)
  (let ((pool (when pool (make-system-session-pool))))
    (cond
      ((or reset (not *system-session*)) 
       (setf *system-session* (make-system-session :pool pool)
             *system-table* (make-hash-table)
             *module* nil
             *module-table* (make-hash-table :test 'equal)))
      (pool
       (setf (system-session-pool *system-session*) pool))))
  (when (and sysdefs preload) 
    (mapc (lambda (x)
            (when *verbose* (mumble "loading systems from ~A" x))
            (load-sys x))
          *sysdefs*))
  (values))

(definline %load-system (sys &optional force)
  (declare (optimize (speed 3)))
  (when (or (component-reload-p sys) force)
    (with-system-file ((path sys) :load t)
      ;; (when verbose (mumble "Loading system ~A~@[ from ~A~]" (name sys) path))
      (load-component sys :force force))
    sys))

(defun reload-system-packages (name &optional (path :pkg))
  (let ((*module* name))
    ;; delete :packages
    (setf (find-module name :packages) nil)
    ;; reload
    (load-component (find-component path (find-system name)) :force t)))

(defun load-system-requires (sys &optional force)
  (mapc
   (lambda (x)
     (if (atom x)
         (if-let ((s (find-system x)))
           (when (component-reload-p s)
             (%load-system s force))
           (or (apply 'load-module (mapcar 'keywordicate (split-sequence #\/ (string-upcase x))))
               (simple-system-error "System not found: ~A" x)))
         (apply 'load-module x)))
   (slot-value sys 'std/defsys::require)))

(defun call-system-providers (sys)
  (let ((*module* (name sys)))
    (mapc (lambda (x) (call-provider (car x) (cons *module* (cdr x))))
          (slot-value sys 'provide))))

(defmethod init ((self system) &key force)
  "Initialize a SYSTEM which has been pre-loaded with LOAD-SYS. Arrange for
REQUIRE forms, PKG components, and PROVIDE forms to be loaded. The underlying
object SELF remains unmodified."
  ;; first process all requires
  (load-system-requires self force)
  ;; initialize system hooks (call :init hooks)
  (init-module self)
  ;; then we call providers
  (call-system-providers self)
  ;; and set variables
  (unless (typep self 'test-system) (setq *module* (name self)))
  self)

(defmethod reset ((self system) &key)
  (let ((sys (path self))
        (name (name self)))
    (delete-system self)
    (load-sys sys name)))

;; (typecase x
;;   ;; symbols and strings use PROVIDE
;;   ((or symbol simple-string) (provide x) x)
;;   ;; WARNING: use of eval
;;   (list (with-safe-io-syntax (:std/defsys) (eval (cdr x))))
;;   ;; otherwise return as-is
;;   (t x)))
;; (slot-value self 'provide)))

(defgeneric find-system (self &key &allow-other-keys)
  (:method ((self t) &key default (asdf *asdf-compatibility*))
    (multiple-value-bind (val found) (gethash (keywordicate (string-upcase (name (ensure-car self)))) *system-table*)
      (cond
        (found (values val found))
        (asdf (asdf:find-system self (eql default :error)))
        ((eql default :error) (simple-system-error "System ~A not found." self))
        ((functionp default) (funcall default))
        (t default)))))

(defgeneric remove-system (self &key &allow-other-keys)
  (:method ((self system) &rest args)
    (apply 'remove-system (name self) args))
  (:method ((self t) &key)
    (with-system-session ()
      ;; TODO 2026-02-26: purge caches
      (remhash self *system-table*))))

(defgeneric load-system (self &key &allow-other-keys)
  (:documentation "Load the system SELF by ensuring all dependencies and components are loaded.")
  (:method ((self system) &key force (verbose *verbose*) (asdf *asdf-compatibility*) (init t) tests)
    (or
     (with-system-session (_ self)
       (declare (ignore _))
       (when init (init self :force force))
       ;; call the load hook
       (funcall (the function (hook self)) :load)
       ;; TODO 2025-08-31:
       ;; - build-plan
       (prog1 (case (plan self)
                ((or :serial nil) (%load-system self force))
                (t (nyi! "Unrecognized PLAN keyword")))
         (when tests (load-module (name self) :tests))))
     (and asdf (asdf:load-system (name self) :verbose verbose :force force)))
    (values self (find-module (name self))))
  (:method (self &rest args &key (default :error) (asdf *asdf-compatibility*))
    (remf args :default)
    (let ((sys (find-system self :default default :asdf asdf)))
      (apply 'load-system sys args)))
  (:method ((self asdf:system) &rest args)
    (apply 'asdf:load-system self args)))

(defgeneric compile-system (self &key &allow-other-keys)
  (:documentation "Compile system SELF.")
  (:method ((self system) &key (asdf *asdf-compatibility*) (verbose *verbose*) (init t) force)
    (or         
     (with-system-session ()
       (compile-sys (keywordicate (name self)) force)
       (when init (init self))
       (when verbose (mumble "Compiling system ~A" (name self)))
       (compile-component self :verbose verbose :force force))
     (and asdf (asdf:compile-system (name self) :verbose verbose :force force))))
  (:method ((self symbol) &rest args &key (default :error))
    (remf args :default)
    (apply 'compile-system (find-system self :default default) args)))

(defgeneric save-system (self &key &allow-other-keys)
  (:documentation "Save the system SELF by loading it then calling SAVE-LISP with supplied args.")
  (:method ((self system) &rest args)
    (load-system self)
    (let ((name (string-downcase (or (getf args :name) (name self)))))
      (apply 'std:save-lisp (merge-pathnames name (system-data-dir "bin/")) (std/list:remove-from-plist args :name)))))

(defgeneric make-system (self &rest args &key &allow-other-keys)
  (:documentation "Make the system SELF which usually entails loading, compiling, and then saving
an image. The PROVIDE slot of SELF is scanned for relevant modules given supplied args.")
  (:method ((self system) &rest args &key (bin t))
    (let ((args (std/list:remove-from-plist args :bin)))
      (apply 'compile-system self args)
      (apply 'load-system self args)
      (if-let ((bin (and bin (find-module (name self) :bin))))
        (funcall (the function bin))
        (apply 'save-system self args))))
  (:method ((self symbol) &key)
    (let ((sys (find-system self :default :error)))
      (make-system sys))))

(defgeneric fetch-system (self &key &allow-other-keys)
  (:documentation "Fetch a system from a remote location."))

(defgeneric update-system (self &key &allow-other-keys)
  (:documentation "Update the system SELF."))

(defgeneric delete-system (self &key &allow-other-keys)
  (:documentation "Delete the system SELF from the Lisp image, cache, or local filesystem.")
  (:method ((self system) &key)
    (remove-system self)
    ;; todo: remove-module
    (remhash (name self) *module-table*)
    ;; todo: purge/protect
    ;; (when cache
    ))

(defgeneric test-system (self &rest args)
  (:documentation "Test the system SELF.")
  (:method ((self system) &rest args &key (asdf *asdf-compatibility*))
    (remf args :asdf)
    (mumble "Testing system ~A" (name self))
    (if asdf
        (asdf:test-system self args)
        (progn (load-module (name self) :tests)
               (apply 'pkg:symbol-call *test-system* 'do-tests :suite (name self) args))))
  (:method ((self symbol) &rest args)
    (let ((sys (find-system self :default :error)))
      (apply #'test-system sys args))))

;;; Explorer
(defmethod explore ((self system) &key)
  "Explore a system in the Lisp REPL.")
