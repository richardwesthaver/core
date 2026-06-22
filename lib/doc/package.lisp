;;; lib/doc/package.lisp --- Package Documentation

;; Package documentation abstractions and machinery

;;; Commentary:

;; We usually think of packages as composed of one or more files, but
;; this is not always the case in Lisp. Packages can be defined in a
;; REPL with no underlying source files, or via macros, which can
;; obfuscate the origin of a form.

;; The good news is that packages are 'real' objects that are exposed
;; to us after load-time. If we are willing to wait for the packages
;; to actually be loaded in a Lisp image before attempting to compile
;; 'package documentation' this makes everything incredibly easy with
;; SB-INTROSPECT and friends.

;; All that remains is to provide an interface for linking the various
;; downstream *-DOCUMENTATION objects with a compiled
;; PACKAGE-DEFINITION object.

;; The logical next step is linking PACKAGE-DOCUMENTATION objects with
;; other PACKAGE-DEFINITIONs.

;;; Code:
(in-package :doc)

(deftempo :package-documentation
  "* <%@var name%>
:PROPERTIES:
:SUMMARY: <%@var summary%>
:LOCATION: <%@var location%>
:END:
<%@var description%>
<%@var info%>
<%@ifnotempty external%>
* External Symbols
<%@loop external%>

<%@endloop%>
<%@endif%><%@ifnotempty internal%>
* Internal Symbols
<%@loop internal%>

<%@endloop%>
<%@endif%>")

(defclass package-documentation (id)
  ((package :initform *package* :initarg :package :type package :accessor doc-object)
   (files :initform nil :initarg :files :accessor doc-files)
   (symbols :initform #() :initarg :symbols :type (vector symbol-documentation) :accessor doc-symbols)))

(defmethod name ((self package-documentation))
  (package-name (doc-object self)))

(defun package-documentation (&optional (package *package*) (for :external) (safe-directories (project-directories)))
  "Return a PACKAGE-DOCUMENTATION object from PACKAGE."
  (unless (packagep package)
    (if (or (null package) (eq t package))
        (setf package *package*)
        (setf package (find-package package))))
  (let ((paths)
        (symbols (make-array (package-external-symbol-count package)
                             :element-type 'symbol-documentation
                             :fill-pointer 0)))
    ;; TODO: we always want external symbols, we need XOR
    (case for
      (:internal (do-symbols* (s package)
                   (let ((doc (symbol-documentation s)))
                     (dolist (p (doc-files doc))
                       (pushnew p paths))
                     (vector-push-extend doc symbols))))
      (:external (do-external-symbols (s package)
                   (let ((doc (symbol-documentation s)))
                     (dolist (p (doc-files doc))
                       (pushnew p paths))
                     (vector-push doc symbols))))
      (t (loop for s being each present-symbol in package
               do (let ((doc (symbol-documentation s)))
                    (dolist (p (doc-files doc))
                      (unless (null p)
                        (pushnew p paths)))
                    (vector-push doc symbols)))))
    (make-instance 'package-documentation
      :id (make-v5-uuid +namespace-oid+ (package-name package))
      :package package
      :files (mapcan
              (lambda (x) 
                (unless (notany 
                         (lambda (y) 
                           (pathname-match-p
                            x
                            (make-pathname :directory `(,@y :wild-inferiors))))
                         safe-directories)
                  (list (file-documentation x))))
              paths)
      :symbols symbols)))

(defmethod print-object ((self package-documentation) stream)
  (with-slots (package files symbols) self
    (print-unreadable-object (self stream :type t)
      (format stream "~A :symbols ~A :files ~A" (package-name package) (length symbols) (length files)))))

(defmethod describe-object ((self package-documentation) stream)
  (with-slots (package files symbols) self
    (print-standard-describe-header self stream)
    (describe package stream)
    (format stream "~%Files: ~S"
            (loop for f in files
                  collect (path f)))
    (format stream "~%Symbol Docs: ")
    (pprint-tabular
     stream 
     (loop for s across symbols
           collect (doc-object s)))))

(defmethod dependents ((self package-documentation))
  (mapcar #'package-documentation (package-used-by-list (doc-object self))))

(defmethod dependencies ((self package-documentation))
  (mapcar #'package-documentation (package-use-list (doc-object self))))

;; (sb-introspect:allocation-information (make-instance 'package-documentation))
;; sb-introspect:definition-source

;; (sb-introspect::object-size-histogram :static)
;; (sb-introspect:find-definition-source (find-package :doc))
;; (sb-introspect:find-definition-sources-by-name 'std-error :condition)

;; (package-documentation)

(defmethod publish ((self package-documentation) &key output)
  (with-slots (id files symbols) self
    (let ((gen (execute-template (keywordicate (class-name (class-of self)))
                                 :env
                                 `(:name ,(name self) :id ,id
                                   ;; :tags ,(package-tag-string self)
                                   :files ,files
                                   :symbols ,symbols))))
      (case output
        ('nil (values (org-parse (document-keyword self) gen) gen))
        (:string gen)
        (t (write-string gen output))))))
