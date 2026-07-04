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
  "<%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%> <%@var name%>
:PROPERTIES:
<%@ifnotempty summary%>:SUMMARY: <%@var summary%>
<%@endif%><%@ifnotempty location%>:LOCATION: <%@var location%>
<%@endif%>:END:<%@ifnotempty description%>
<%@var description%>
<%@endif%><%@if info%>
#+call:lisp-package-dependencies(\"<%=env%>\")
#+call:lisp-package-dependents(\"<%=env%>\")<%@endif%><%@ifnotempty symbols%>
<%@loop symbols%>
<%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%>*<%=(doc:publish env :output :string :level 3)%><%@endloop%><%@endif%>")

(defclass package-documentation (document)
  ((package :initform *package* :initarg :package :type package :accessor doc-object)
   (files :initform nil :initarg :files :accessor doc-files)))

(defmethod name ((self package-documentation))
  (package-name (doc-object self)))

(defun package-documentation (&optional (package *package*) (for :external) (safe-directories (project-directories)))
  "Return a PACKAGE-DOCUMENTATION object from PACKAGE."
  (unless (packagep package)
    (if (or (null package) (eq t package))
        (setf package *package*)
        (setf package (find-package package))))
  (let ((paths)
        (symbols))
    ;; TODO: we always want external symbols, we need XOR
    (case for
      (:internal (do-symbols* (s package)
                   (let ((doc (symbol-documentation s)))
                     (dolist (p (doc-files doc))
                       (pushnew p paths))
                     (push doc symbols))))
      (:external (do-external-symbols (s package)
                   (let ((doc (symbol-documentation s)))
                     (dolist (p (doc-files doc))
                       (pushnew p paths))
                     (push doc symbols))))
      (t (loop for s being each present-symbol in package
               do (let ((doc (symbol-documentation s)))
                    (dolist (p (doc-files doc))
                      (unless (null p)
                        (pushnew p paths)))
                    (push doc symbols)))))
    (make-instance 'package-documentation
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
      :ast symbols)))

(defmethod print-object ((self package-documentation) stream)
  (with-slots (package ast) self
    (print-unreadable-object (self stream :type t)
      (format stream "~A :symbols ~A" (package-name package) (length ast)))))

(defmethod describe-object ((self package-documentation) stream)
  (with-slots (package ast) self
    (print-standard-describe-header self stream)
    (describe package stream)
    (format stream "~%Symbol Docs: ")
    (pprint-tabular
     stream 
     (loop for s in ast
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

(defmethod publish ((self package-documentation) &key output level)
  (with-slots (package ast) self
    (let ((gen (execute-template (keywordicate (class-name (class-of self)))
                                 :env
                                 `(:name ,(name self) :id ,(id package)
                                         ;; :tags ,(package-tag-string self)
                                         ,@(when level `(:level ,level))
                                         :symbols ,(mapcan (lambda (x) (and (home-package-p (doc-object x) (name self)) (list x))) ast)))))
      (case output
        ('nil gen)
        (:string gen)
        (t (if (pathnamep output)
               (with-output-to-file (f output)
                 (write-string gen f))
               (write-string gen output)))))))
