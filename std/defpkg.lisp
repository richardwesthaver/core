;;; defpkg.lisp --- defpackage extension macro

;; DEFPKG is based on UIOP:DEFINE-PACKAGE

;;; Commentary:

;;

;;; Code:
(defpackage :std/defpkg
  (:use :cl)
  (:nicknames :pkg)
  (:export :defpkg
   :find-package* :find-symbol* :symbol-call :intern*
   :export* :import* :shadowing-import* :shadow* 
   :symbol-shadowing-p :home-package-p :make-symbol* :unintern*
   :symbol-package-name :standard-common-lisp-symbol-p
   :reify-package :unreify-package :reify-symbol :unreify-symbol
   :nuke-symbol-in-package :nuke-symbol :rehome-symbol :ensure-package-unused
   :delete-package* :package-names :packages-from-names :fresh-package-name 
   :rename-package-away :package-definition-form :parse-defpkg-form :ensure-package
   :with-package :define-lisp-package
   :defpackage* :*default-package* :*defpkg-hook* :package-symbols-except
   :*default-package-file-name*
   :test-package-p
   :module-package-p
   :internal-package-p
   :*component-packages* :*default-pkg-component-use*))

(in-package :std/defpkg)

(defvar *default-package* "CL-USER")

(defvar *default-package-file-name* "pkg"
  "The file-name in a system definition which is expected to contain package
definitions.")

(defparameter *defpkg-hook* nil)

(defparameter *component-packages* nil)

(defvar *default-pkg-component-use* '(#:cl #:std/defpkg))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun find-package* (package-designator &optional (error t))
    "Alternative to FIND-PACKAGE with optional error."
    (let ((package (find-package package-designator)))
      (cond
        (package package)
        (error (error "No package named ~S" (string package-designator)))
        (t nil))))
  (defun find-symbol* (name package-designator &optional (error t))
    "Find a symbol in a package of given string'ified NAME;
unlike CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package ;; package error handled by find-package* already
          (multiple-value-bind (symbol status) (find-symbol (string name) package)
            (cond
              (status (return (values symbol status)))
              (error (error "There is no symbol ~S in package ~S" name (package-name package))))))
        (values nil nil))))
  (defun symbol-call (package name &rest args)
    "Call a function associated with symbol of given name in given package,
with given ARGS. Useful when the call is read before the package is loaded,
or when loading the package is optional."
    (apply (find-symbol* name package) args))
  (defun intern* (name package-designator &optional (error t))
    "Like INTERN but never create a new package."
    (intern (string name) (find-package* package-designator error)))
  (defun export* (name package-designator)
    "Like EXPORT but return NIL on error."
    (let* ((package (find-package* package-designator))
           (symbol (intern* name package)))
      (export (or symbol (list symbol)) package)))
  (defun import* (symbol package-designator)
    "Like IMPORT but return NIL on error."
    (import (or symbol (list symbol)) (find-package* package-designator)))
  (defun shadowing-import* (symbol package-designator)
    "Like SHADOWING-IMPORT but return NIL on error."
    (shadowing-import (or symbol (list symbol)) (find-package* package-designator)))
  (defun shadow* (name package-designator)
    "Like SHADOW but return NIL on error."
    (shadow (list (string name)) (find-package* package-designator)))
  (defun make-symbol* (name)
    "Like MAKE-SYMBOL but when NAME is a symbol and not a string, call COPY-SYMBOL
instead."
    (etypecase name
      (string (make-symbol name))
      (symbol (copy-symbol name))))
  (defun unintern* (name package-designator &optional (error t))
    "Like UNINTERN but with optional errors."
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package
          (multiple-value-bind (symbol status) (find-symbol* name package error)
            (cond
              (status (unintern symbol package)
                      (return (values symbol status)))
              (error (error "symbol ~A not present in package ~A"
                            (string symbol) (package-name package))))))
        (values nil nil))))
  (defun symbol-shadowing-p (symbol package)
    "Return T if SYMBOL is a member PACKAGE's shadowing symbols."
    (and (member symbol (package-shadowing-symbols package)) t))
  (defun home-package-p (symbol package)
    "Return T if PACKAGE is the 'home package' of SYMBOL."
    (and package (let ((sp (symbol-package symbol)))
                   (and sp (let ((pp (find-package* package)))
                             (and pp (eq sp pp))))))))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun symbol-package-name (symbol)
    "Return the name of the package SYMBOL belongs to."
    (let ((package (symbol-package symbol)))
      (and package (package-name package))))
  (defun standard-common-lisp-symbol-p (symbol)
    "Return T if SYMBOL is a standard COMMON-LISP symbol."
    (multiple-value-bind (sym status) (find-symbol* symbol :common-lisp nil)
      (and (eq sym symbol) (eq status :external))))
  (defun reify-package (package &optional package-context)
    "Return the appropriate relative value of PACKAGE in optional PACKAGE-CONTEXT."
    (if (eq package package-context) t
        (etypecase package
          (null nil)
          ((eql (find-package :cl)) :cl)
          (package (package-name package)))))
  (defun unreify-package (package &optional package-context)
    "Return the concrete value of PACKAGE modulo PACKAGE-CONTEXT."
    (etypecase package
      (null nil)
      ((eql t) package-context)
      ((or symbol string) (find-package package))))
  (defun reify-symbol (symbol &optional package-context)
    "Return the appropriate relative value of SYMBOL in optional PACKAGE-CONTEXT."
    (etypecase symbol
      ((or keyword (satisfies standard-common-lisp-symbol-p)) symbol)
      (symbol (vector (symbol-name symbol)
                      (reify-package (symbol-package symbol) package-context)))))
  (defun unreify-symbol (symbol &optional package-context)
    "Return the concrete value of SYMBOL modulo PACKAGE-CONTEXT."
    (etypecase symbol
      (symbol symbol)
      ((simple-vector 2)
       (let* ((symbol-name (svref symbol 0))
              (package-foo (svref symbol 1))
              (package (unreify-package package-foo package-context)))
         (if package (intern* symbol-name package)
             (make-symbol* symbol-name)))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *all-package-happiness* '())
  (defvar *all-package-fishiness* (list t))
  (defun record-fishy (info)
    ;;(format t "~&FISHY: ~S~%" info)
    (push info *all-package-fishiness*))
  (defmacro when-package-fishiness (&body body)
    `(when *all-package-fishiness* ,@body))
  (defmacro note-package-fishiness (&rest info)
    `(when-package-fishiness (record-fishy (list ,@info)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun set-dummy-symbol (symbol reason other-symbol)
    "Set the DUMMY-SYMBOL prop of SYMBOL to (REASON . OTHER-SYMBOL)."
    (setf (get symbol 'dummy-symbol) (cons reason other-symbol)))
  (defun make-dummy-symbol (symbol)
    "Make a DUMMY-SYMBOL and set the prop value of SYMBOL."
    (let ((dummy (copy-symbol symbol)))
      (set-dummy-symbol dummy 'replacing symbol)
      (set-dummy-symbol symbol 'replaced-by dummy)
      dummy))
  (defun dummy-symbol (symbol)
    "Return the DUMMY-SYMBOL prop of SYMBOL."
    (get symbol 'dummy-symbol))
  (defun get-dummy-symbol (symbol)
    "Get the DUMMY-SYMBOL prop of SYMBOL returning values (SYMBOL REASON) if it
exists and setting it if not."
    (let ((existing (dummy-symbol symbol)))
      (if existing (values (cdr existing) (car existing))
          (make-dummy-symbol symbol))))
  (defun nuke-symbol-in-package (symbol package-designator)
    "Nuke SYMBOL in package PACKAGE-DESIGNATOR."
    (let ((package (find-package* package-designator))
          (name (symbol-name symbol)))
      (multiple-value-bind (sym stat) (find-symbol name package)
        (when (and (member stat '(:internal :external)) (eq symbol sym))
          (if (symbol-shadowing-p symbol package)
              (shadowing-import* (get-dummy-symbol symbol) package)
              (unintern* symbol package))))))
  (defun nuke-symbol (symbol &optional (packages (list-all-packages)))
    "Nuke SYMBOL form PACKAGES."
    (loop :for p :in packages :do (nuke-symbol-in-package symbol p)))
  (defun rehome-symbol (symbol package-designator)
    "Changes the home package of a symbol, also leaving it present in its old home if any"
    (let* ((name (symbol-name symbol))
           (package (find-package* package-designator))
           (old-package (symbol-package symbol))
           (old-status (and old-package (nth-value 1 (find-symbol name old-package))))
           (shadowing (and old-package (symbol-shadowing-p symbol old-package) (make-symbol name))))
      (multiple-value-bind (overwritten-symbol overwritten-symbol-status) (find-symbol name package)
        (unless (eq package old-package)
          (let ((overwritten-symbol-shadowing-p
                  (and overwritten-symbol-status
                       (symbol-shadowing-p overwritten-symbol package))))
            (note-package-fishiness
             :rehome-symbol name
             (when old-package (package-name old-package)) old-status (and shadowing t)
             (package-name package) overwritten-symbol-status overwritten-symbol-shadowing-p)
            (when old-package
              (if shadowing
                  (shadowing-import* shadowing old-package))
              (unintern* symbol old-package))
            (cond
              (overwritten-symbol-shadowing-p
               (shadowing-import* symbol package))
              (t
               (when overwritten-symbol-status
                 (unintern* overwritten-symbol package))
               (import* symbol package)))
            (if shadowing
                (shadowing-import* symbol old-package)
                (import* symbol old-package))
            (when (eq old-status :external)
              (export* symbol old-package))
            (when (eq overwritten-symbol-status :external)
              (export* symbol package))))
        (values overwritten-symbol overwritten-symbol-status))))
  (defun ensure-package-unused (package)
    "Ensure that PACKAGE is unused by any other package."
    (loop :for p :in (package-used-by-list package) :do
             (unuse-package package p)))
  (defun delete-package* (package &key nuke)
    "Delete package PACKAGE, additionally ensuring it is unused by other packages
and optionally nuking all symbols as well."
    (let ((p (find-package package)))
      (when p
        (when nuke (do-symbols (s p) (when (home-package-p s p) (nuke-symbol s))))
        (ensure-package-unused p)
        (delete-package package))))
  (defun package-names (package)
    "Return a cons of PACKAGE name and nicknames."
    (cons (package-name package) (package-nicknames package)))
  (defun packages-from-names (names)
    "Return packages associated with list NAMES, starting from the end and deleting all duplicates."
    (remove-duplicates (remove nil (mapcar #'find-package names)) :from-end t))
  (defun fresh-package-name (&key (prefix :%TO-BE-DELETED)
                                  separator
                                  (index (random most-positive-fixnum)))
    (loop :for i :from index
          :for n = (format nil "~A~@[~A~D~]" prefix (and (plusp i) (or separator "")) i)
          :thereis (and (not (find-package n)) n)))
  (defun rename-package-away (p &rest keys &key prefix &allow-other-keys)
    (let ((new-name
            (apply 'fresh-package-name
                   :prefix (or prefix (format nil "__~A__" (package-name p))) keys)))
      (record-fishy (list :rename-away (package-names p) new-name))
      (rename-package p new-name))))


;;; Communicable representation of symbol and package information
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun package-definition-form (package-designator
                                  &key (nicknamesp t) (usep t)
                                       (shadowp t) (shadowing-import-p t)
                                       (exportp t) (importp t) internp (error t))
    (let* ((package (or (find-package* package-designator error)
                        (return-from package-definition-form nil)))
           (name (package-name package))
           (nicknames (package-nicknames package))
           (use (mapcar #'package-name (package-use-list package)))
           (shadow ())
           (shadowing-import (make-hash-table :test 'equal))
           (import (make-hash-table :test 'equal))
           (export ())
           (intern ()))
      (when package
        (loop :for sym :being :the :symbols :in package
              :for status = (nth-value 1 (find-symbol* sym package)) :do
                 (ecase status
                   ((nil :inherited))
                   ((:internal :external)
                    (let* ((name (symbol-name sym))
                           (external (eq status :external))
                           (home (symbol-package sym))
                           (home-name (package-name home))
                           (imported (not (eq home package)))
                           (shadowing (symbol-shadowing-p sym package)))
                      (cond
                        ((and shadowing imported)
                         (push name (gethash home-name shadowing-import)))
                        (shadowing
                         (push name shadow))
                        (imported
                         (push name (gethash home-name import))))
                      (cond
                        (external
                         (push name export))
                        (imported)
                        (t (push name intern)))))))
        (labels ((sort-names (names)
                   (sort (copy-list names) #'string<))
                 (table-keys (table)
                   (loop :for k :being :the :hash-keys :of table :collect k))
                 (when-relevant (key value)
                   (when value (list (cons key value))))
                 (import-options (key table)
                   (loop :for i :in (sort-names (table-keys table))
                         :collect `(,key ,i ,@(sort-names (gethash i table))))))
          `(defpackage ,name
             ,@(when-relevant :nicknames (and nicknamesp (sort-names nicknames)))
             (:use ,@(and usep (sort-names use)))
             ,@(when-relevant :shadow (and shadowp (sort-names shadow)))
             ,@(import-options :shadowing-import-from (and shadowing-import-p shadowing-import))
             ,@(import-options :import-from (and importp import))
             ,@(when-relevant :export (and exportp (sort-names export)))
             ,@(when-relevant :intern (and internp (sort-names intern)))))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun ensure-shadowing-import (name to-package from-package shadowed imported)
    (check-type name string)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (let ((import-me (find-symbol* name from-package)))
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (cond
          ((gethash name shadowed)
           (unless (eq import-me existing)
             (error "Conflicting shadowings for ~A" name)))
          (t
           (setf (gethash name shadowed) t)
           (setf (gethash name imported) t)
           (unless (or (null status)
                       (and (member status '(:internal :external))
                            (eq existing import-me)
                            (symbol-shadowing-p existing to-package)))
             (note-package-fishiness
              :shadowing-import name
              (package-name from-package)
              (or (home-package-p import-me from-package) (symbol-package-name import-me))
              (package-name to-package) status
              (and status (or (home-package-p existing to-package) (symbol-package-name existing)))))
           (shadowing-import* import-me to-package))))))
  (defun ensure-imported (import-me into-package &optional from-package)
    (check-type import-me symbol)
    (check-type into-package package)
    (check-type from-package (or null package))
    (let ((name (symbol-name import-me)))
      (multiple-value-bind (existing status) (find-symbol name into-package)
        (cond
          ((not status)
           (import* import-me into-package))
          ((eq import-me existing))
          (t
           (let ((shadowing-p (symbol-shadowing-p existing into-package)))
             (note-package-fishiness
              :ensure-imported name
              (and from-package (package-name from-package))
              (or (home-package-p import-me from-package) (symbol-package-name import-me))
              (package-name into-package)
              status
              (and status (or (home-package-p existing into-package) (symbol-package-name existing)))
              shadowing-p)
             (cond
               ((or shadowing-p (eq status :inherited))
                (shadowing-import* import-me into-package))
               (t
                (unintern* existing into-package)
                (import* import-me into-package))))))))
    (values))
  (defun ensure-import (name to-package from-package shadowed imported)
    (check-type name string)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (multiple-value-bind (import-me import-status) (find-symbol name from-package)
      (when (null import-status)
        (note-package-fishiness
         :import-uninterned name (package-name from-package) (package-name to-package))
        (setf import-me (intern* name from-package)))
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (cond
          ((and imported (gethash name imported))
           (unless (and status (eq import-me existing))
             (error "Can't import ~S from both ~S and ~S"
                    name (package-name (symbol-package existing)) (package-name from-package))))
          ((gethash name shadowed)
           (error "Can't both shadow ~S and import it from ~S" name (package-name from-package)))
          (t
           (setf (gethash name imported) t))))
      (ensure-imported import-me to-package from-package)))
  (defun ensure-inherited (name symbol to-package from-package mixp shadowed imported inherited)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type from-package package)
    (check-type mixp boolean)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (multiple-value-bind (existing status) (find-symbol name to-package)
      (let* ((sp (symbol-package symbol))
             (in (gethash name inherited))
             (xp (and status (symbol-package existing))))
        (when (null sp)
          (note-package-fishiness
           :import-uninterned name
           (package-name from-package) (package-name to-package) mixp)
          (import* symbol from-package)
          (setf sp (package-name from-package)))
        (cond
          ((gethash name shadowed))
          (in
           (unless (equal sp (first in))
             (if mixp
                 (ensure-shadowing-import name to-package (second in) shadowed imported)
                 (error "Can't inherit ~S from ~S, it is inherited from ~S"
                        name (package-name sp) (package-name (first in))))))
          ((gethash name imported)
           (unless (eq symbol existing)
             (error "Can't inherit ~S from ~S, it is imported from ~S"
                    name (package-name sp) (package-name xp))))
          (t
           (setf (gethash name inherited) (list sp from-package))
           (when (and status (not (eq sp xp)))
             (let ((shadowing (symbol-shadowing-p existing to-package)))
               (note-package-fishiness
                :inherited name
                (package-name from-package)
                (or (home-package-p symbol from-package) (symbol-package-name symbol))
                (package-name to-package)
                (or (home-package-p existing to-package) (symbol-package-name existing)))
               (if shadowing (ensure-shadowing-import name to-package from-package shadowed imported)
                   (unintern* existing to-package)))))))))
  (defun ensure-mix (name symbol to-package from-package shadowed imported inherited)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (unless (gethash name shadowed)
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (let* ((sp (symbol-package symbol))
               (im (gethash name imported))
               (in (gethash name inherited)))
          (cond
            ((or (null status)
                 (and status (eq symbol existing))
                 (and in (eq sp (first in))))
             (ensure-inherited name symbol to-package from-package t shadowed imported inherited))
            (in
             (remhash name inherited)
             (ensure-shadowing-import name to-package (second in) shadowed imported))
            (im
             (error "Symbol ~S import from ~S~:[~; actually ~:[uninterned~;~:*from ~S~]~] conflicts with existing symbol in ~S~:[~; actually ~:[uninterned~;from ~:*~S~]~]"
                    name (package-name from-package)
                    (home-package-p symbol from-package) (symbol-package-name symbol)
                    (package-name to-package)
                    (home-package-p existing to-package) (symbol-package-name existing)))
            (t
             (ensure-inherited name symbol to-package from-package t shadowed imported inherited)))))))

  (defun recycle-symbol (name recycle exported)
    "Takes a symbol NAME (a string), a list of package designators for RECYCLE
packages, and a hash-table of names (strings) of symbols scheduled to be
EXPORTED from the package being defined. It returns two values, the symbol
found (if any, or else NIL), and a boolean flag indicating whether a symbol
was found. The caller (DEFPKG) will then do the re-homing of the symbol, etc."
    (check-type name string)
    (check-type recycle list)
    (check-type exported hash-table)
    (when (gethash name exported) ;; don't bother recycling private symbols
      (let (recycled foundp)
        (dolist (r recycle (values recycled foundp))
          (multiple-value-bind (symbol status) (find-symbol name r)
            (when (and status (home-package-p symbol r))
              (cond
                (foundp
                 ;; (nuke-symbol symbol)) -- even simple variable names like O or C will do that.
                 (note-package-fishiness :recycled-duplicate name (package-name foundp) (package-name r)))
                (t
                 (setf recycled symbol foundp r)))))))))
  (defun symbol-recycled-p (sym recycle)
    (check-type sym symbol)
    (check-type recycle list)
    (and (member (symbol-package sym) recycle) t))
  (defun ensure-symbol (name package intern recycle shadowed imported inherited exported)
    (check-type name string)
    (check-type package package)
    (check-type intern boolean)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (unless (or (gethash name shadowed)
                (gethash name imported)
                (gethash name inherited))
      (multiple-value-bind (existing status)
          (find-symbol name package)
        (multiple-value-bind (recycled previous) (recycle-symbol name recycle exported)
          (cond
            ((and status (eq existing recycled) (eq previous package)))
            (previous
             (rehome-symbol recycled package))
            ((and status (eq package (symbol-package existing))))
            (t
             (when status
               (note-package-fishiness
                :ensure-symbol name
                (reify-package (symbol-package existing) package)
                status intern)
               (unintern existing))
             (when intern
               (intern* name package))))))))
  (declaim (ftype (function (t t t &optional t) t) ensure-exported))
  (defun ensure-exported-to-user (name symbol to-package &optional recycle)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type recycle list)
    (assert (equal name (symbol-name symbol)))
    (multiple-value-bind (existing status) (find-symbol name to-package)
      (unless (and status (eq symbol existing))
        (let ((accessible
                (or (null status)
                    (let ((shadowing (symbol-shadowing-p existing to-package))
                          (recycled (symbol-recycled-p existing recycle)))
                      (unless (and shadowing (not recycled))
                        (note-package-fishiness
                         :ensure-export name (symbol-package-name symbol)
                         (package-name to-package)
                         (or (home-package-p existing to-package) (symbol-package-name existing))
                         status shadowing)
                        (if (or (eq status :inherited) shadowing)
                            (shadowing-import* symbol to-package)
                            (unintern existing to-package))
                        t)))))
          (when (and accessible (eq status :external))
            (ensure-exported name symbol to-package recycle))))))
  (defun ensure-exported (name symbol from-package &optional recycle)
    (dolist (to-package (package-used-by-list from-package))
      (ensure-exported-to-user name symbol to-package recycle))
    (unless (eq from-package (symbol-package symbol))
      (ensure-imported symbol from-package))
    (export* name from-package))
  (defun ensure-export (name from-package &optional recycle)
    (multiple-value-bind (symbol status) (find-symbol* name from-package)
      (unless (eq status :external)
        (ensure-exported name symbol from-package recycle))))

  (defun ensure-package (name &key
                              nicknames documentation use
                              shadow shadowing-import-from
                              import-from export intern
                              recycle mix reexport
                              unintern package-local-nicknames lock implements)
    (let* ((package-name (string name))
	   (nicknames (mapcar #'string nicknames))
           (names (cons package-name nicknames))
           (previous (packages-from-names names))
           (discarded (cdr previous))
           (to-delete ())
           (package (or (first previous) (make-package package-name :nicknames nicknames)))
           (recycle (packages-from-names recycle))
           (use (mapcar 'find-package* use))
           (mix (mapcar 'find-package* mix))
           (reexport (mapcar 'find-package* reexport))
           (shadow (mapcar 'string shadow))
           (export (mapcar 'string export))
	   (intern (mapcar 'string intern))
	   (implements (mapcar 'find-package* implements))
           (unintern (mapcar 'string unintern))
           (shadowed (make-hash-table :test 'equal)) ; string to bool
           (imported (make-hash-table :test 'equal)) ; string to bool
           (exported (make-hash-table :test 'equal)) ; string to bool
           ;; string to list home package and use package:
           (inherited (make-hash-table :test 'equal)))
      (when-package-fishiness (record-fishy package-name))
      (when documentation (setf (documentation package t) documentation))
      (when lock (sb-ext:lock-package package))
      (loop for p in (set-difference implements (sb-ext:package-implements-list package))
            do (sb-ext:add-implementation-package p package))
      (loop for p in package-local-nicknames
	    do (sb-ext:add-package-local-nickname (pop p) (pop p) package))
      (loop :for p :in (set-difference (package-use-list package) (append mix use))
            :do (note-package-fishiness :over-use name (package-names p))
                (unuse-package p package))
      (loop :for p :in discarded
            :for n = (remove-if #'(lambda (x) (member x names :test 'equal))
                                (package-names p))
            :do (note-package-fishiness :nickname name (package-names p))
                (cond (n (rename-package p (first n) (rest n)))
                      (t (rename-package-away p)
                         (push p to-delete))))
      (rename-package package package-name nicknames)
      (dolist (name unintern)
        (multiple-value-bind (existing status) (find-symbol name package)
          (when status
            (unless (eq status :inherited)
              (note-package-fishiness
               :unintern (package-name package) name (symbol-package-name existing) status)
              (unintern* name package nil)))))
      (dolist (name export)
        (setf (gethash name exported) t))
      (dolist (p reexport)
        (do-external-symbols (sym p)
          (setf (gethash (string sym) exported) t)))
      (do-external-symbols (sym package)
        (let ((name (symbol-name sym)))
          (unless (gethash name exported)
            (note-package-fishiness
             :over-export (package-name package) name
             (or (home-package-p sym package) (symbol-package-name sym)))
            (unexport sym package))))
      (dolist (name shadow)
        (setf (gethash name shadowed) t)
        (multiple-value-bind (existing status) (find-symbol name package)
          (multiple-value-bind (recycled previous) (recycle-symbol name recycle exported)
            (let ((shadowing (and status (symbol-shadowing-p existing package))))
              (cond
                ((eq previous package))
                (previous
                 (rehome-symbol recycled package))
                ((or (member status '(nil :inherited))
                     (home-package-p existing package)))
                (t
                 (let ((dummy (make-symbol name)))
                   (note-package-fishiness
                    :shadow-imported (package-name package) name
                    (symbol-package-name existing) status shadowing)
                   (shadowing-import* dummy package)
                   (import* dummy package)))))))
        (shadow* name package))
      (loop :for (p . syms) :in shadowing-import-from
            :for pp = (find-package* p) :do
               (dolist (sym syms) (ensure-shadowing-import (string sym) package pp shadowed imported)))
      (loop :for p :in mix
            :for pp = (find-package* p) :do
               (do-external-symbols (sym pp) (ensure-mix (symbol-name sym) sym package pp shadowed imported inherited)))
      (loop :for (p . syms) :in import-from
            :for pp = (find-package p) :do
               (dolist (sym syms) (ensure-import (symbol-name sym) package pp shadowed imported)))
      (dolist (p (append use mix))
        (do-external-symbols (sym p) (ensure-inherited (string sym) sym package p nil shadowed imported inherited))
        (use-package p package))
      (loop :for name :being :the :hash-keys :of exported :do
               (ensure-symbol name package t recycle shadowed imported inherited exported)
               (ensure-export name package recycle))
      (dolist (name intern)
        (ensure-symbol name package t recycle shadowed imported inherited exported))
      (do-symbols (sym package)
        (ensure-symbol (symbol-name sym) package nil recycle shadowed imported inherited exported))
      (map () 'delete-package* to-delete)
      package)))

(defmacro %defpkg* (pkg args)
  "Define a new 'prelude' package with NAME (car args) which exports SYMBOLS (cdr
args) from PKG."
  (sb-int:once-only ((a args))
    `(let ((name (car ,a))
           (syms (cdr ,a)))
       (eval `(defpackage ,name
                (:import-from ,,pkg ,@syms)
                (:export ,@syms))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-defpkg-form (package clauses)
    (loop
      :with use-p = nil :with recycle-p = nil
      :with documentation = nil
      :with lock = nil
      :for (kw . args) :in clauses
      :when (eq kw :nicknames) :append args :into nicknames :else
      :when (eq kw :documentation)
      :do (cond
            (documentation (error "defpkg: can't define documentation twice"))
            ((or (atom args) (cdr args)) (error "defpkg: bad documentation"))
            (t (setf documentation (car args)))) 
      :else
      :when (eq kw :use) :append args :into use :and :do (setf use-p t) :else
      :when (eq kw :shadow) :append args :into shadow :else
      :when (eq kw :shadowing-import-from) :collect args :into shadowing-import-from :else
      :when (eq kw :import-from) :collect args :into import-from :else
      :when (eq kw :export) :append args :into export :else
      :when (eq kw :intern) :append args :into intern :else
      :when (eq kw :recycle) :append args :into recycle :and :do (setf recycle-p t) :else
      :when (eq kw :mix) :append args :into mix :else
      :when (eq kw :package-local-nicknames) :collect args :into plns :else
      :when (eq kw :implements) :append args :into implements :else
      :when (eq kw :prelude) :collect args :into preludes :else
      :when (and (eq kw :lock) args) :do (setf lock t) :else
      :when (eq kw :reexport) :append args :into reexport :else
      :when (eq kw :use-reexport) :append args :into use :and :append args :into reexport
      :and :do (setf use-p t) :else
      :when (eq kw :mix-reexport) :append args :into mix :and :append args :into reexport
      :and :do (setf use-p t) :else
      :when (eq kw :unintern) :append args :into unintern :else
      :do (error "unrecognized defpkg keyword ~S" kw)
      :finally 
         (return 
           (values
            `(,package
              :nicknames ,nicknames :documentation ,documentation
              :package-local-nicknames ,plns
              :implements ,implements
              :lock ,lock
              :use ,(if use-p use '(:common-lisp))
              :shadow ,shadow :shadowing-import-from ,shadowing-import-from
              :import-from ,import-from :export ,export :intern ,intern
              :recycle ,(if recycle-p recycle (cons package nicknames))
              :mix ,mix :reexport ,reexport :unintern ,unintern)
            preludes)))))

(defmacro defpkg (package &rest clauses)
  "Richard's Robust DEFPACKAGE macro. Based on UIOP:DEFINE-PACKAGE ymmv.

DEFPKG takes a PACKAGE and a number of CLAUSES, of the form (KEYWORD . ARGS).

DEFPKG supports the following standard extensions:
USE, SHADOW, SHADOWING-IMPORT-FROM, IMPORT-FROM, EXPORT, INTERN -- as per CL:DEFPACKAGE.

As well as the common extensions:
RECYCLE, MIX, REEXPORT, UNINTERN -- as per UIOP/PACKAGE:DEFINE-PACKAGE

Additionally, DEFPKG supports the following combinators:

MIX-REEXPORT -- combination of MIX and REEXPORT

USE-REEXPORT -- combination of USE and REEXPORT

and the following custom extensions:

PRELUDE -- treat the first element as the name of a 'prelude' package which
will be defined with the remaining symbols re-exported from PACKAGE and only
those symbols.

In addition to defining and returning a package, when *DEFPKG-HOOK* is
non-nil, it is called as a function with a single argument - the package being
defined."
  (multiple-value-bind (form preludes) (parse-defpkg-form package clauses)
    (let ((pkg `(apply 'ensure-package ',form))
          (prd (mapcar (lambda (x) `(%defpkg* ',(car form) ',x)) preludes)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (locally
             (declare (sb-ext:muffle-conditions sb-kernel::package-at-variance))
           (handler-bind
               ((sb-kernel::package-at-variance #'muffle-warning))
             (prog1 (if #1=*defpkg-hook*
                        (funcall #1# ,pkg)
                        ,pkg)
               ,@(when prd `(,@prd)))))))))

(defmacro define-lisp-package (package)
  "Define a lisp package based on target PACKAGE which transparently exports all
COMMON-LISP symbols in addition to those already exported by the target
package."
  (flet ((externals-of (pkg)
           (loop for s being each external-symbol in pkg collect s)))
    (let* ((pkg-externs (externals-of package))
           (pkg-shadows (intersection (package-shadowing-symbols package)
                                      pkg-externs))
           (cl-externs (externals-of "COMMON-LISP")))
      `(defpackage ,(sb-int:symbolicate package "-LISP")
         (:use "COMMON-LISP")
         (:shadowing-import-from ,package ,@pkg-shadows)
         (:import-from ,package ,@(set-difference pkg-externs pkg-shadows))
         (:export ,@cl-externs)
         (:export ,@pkg-externs)))))

(defmacro with-package (pkg &body body)
  "Execute BODY within the package PKG."
  `(let ((*package* (find-package ,pkg)))
     ,@body))

;; From C-MERA for internal package (syn/gen/c/sym, etc)
(defmacro defpackage* (name (&key shadow-symbols export-symbols) &body body)
  "defpackage with (:shadow ,@<symbols>) and (:export ,@<symbols>)"
  `(let ((shadow-list (loop for i in (remove-duplicates ,shadow-symbols) collect
                               (intern (format nil "~a" i) :keyword)))
         (export-list (loop for i in (remove-duplicates ,export-symbols) collect
                               (intern (format nil "~a" i) :keyword)))
         (body ',body))
     (eval `(defpackage ,,name
              ,@body
              (:shadow ,@shadow-list)
              (:export ,@export-list)))))

;; Helper function for blacklisting symbols when tracing whole packages.
(defun package-symbols-except (name &rest exceptions)
  (let (symbols
        (package (sb-impl::find-undeleted-package-or-lose name)))
    (do-all-symbols (symbol (find-package name))
      (when (eql package (symbol-package symbol))
        (when (and (fboundp symbol)
                   (not (macro-function symbol))
                   (not (special-operator-p symbol)))
          (push symbol symbols))
        (let ((setf-name `(setf ,symbol)))
          (when (fboundp setf-name)
            (push setf-name symbols)))))
    (set-difference symbols exceptions 
                    :key (lambda (x)
                           (if (consp x)
                               (string (second x))
                               (string x))) 
                    :test #'string-equal)))

;; helpers
(defun internal-package-p (package)
  (search "-INT" (package-name package) :from-end t))

(defun module-package-p (package)
  (find #\/ (package-name package)))

(defun test-package-p (package)
  (search "/TESTS" (package-name package) :from-end t))
