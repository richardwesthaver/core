;;; system.lisp --- Lisp System Documentation

;; Standard System Documentation.

;;; Commentary:

;; This module provides the SYSTEM-DOCUMENTATION class which wraps a
;; STD:SYSTEM and provides a basic documentation-focused API.

;; SYSTEM-DOCUMENTATION is the most high-level 'LISP EXCLUSIVE' documentation
;; class provided - the PROJECT-DOCUMENTATION class follows, being designed
;; for projects of any type.

;;; Code:
(in-package :doc)

(deftempo :system-documentation
  "<%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%> <%@var name%>
:PROPERTIES:
<%@ifnotempty id%>:ID: <%@var id%>
<%@endif%><%@ifnotempty summary%>:SUMMARY: <%@var summary%>
<%@endif%><%@ifnotempty file-description%>:DESCRIPTION: <%@var file-description%>
<%@endif%><%@ifnotempty version%>:VERSION: <%@var version%>
<%@endif%><%@if export-file-name%>:EXPORT_FILE_NAME: <%@var export-file-name%>
<%@endif%>:LOCATION: <%@var location%>
:END:<%@ifnotempty description%>
<%@var description%>
<%@endif%><%@if info%>
#+BEGIN: lisp-system-info :system <%@var name%> :files nil :packages nil :symbols nil
#+END:
<%@endif%><%@ifnotempty commentary%>
<%@var commentary%>
<%@endif%><%@ifnotempty packages%>
<%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%>* Packages
<%@loop packages%><%@if /level%><%@repeat /level%>*<%@endrepeat%><%@else%>*<%@endif%><%=(doc:publish env :output :string :level 2)%>
<%@endloop%><%@endif%><%@ifnotempty components%>
<%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%>* Components
<%@loop components%><%@if /level%><%@repeat /level%>*<%@endrepeat%><%@else%>*<%@endif%><%=(doc:publish env :output :string :level 2)%>
<%@endloop%><%@endif%><%@ifnotempty provide%>
<%@if /level%><%@repeat /level%>*<%@endrepeat%><%@else%>*<%@endif%>* Modules
<%@loop provide%><%@if /level%><%@repeat /level%>*<%@endrepeat%><%@else%><%@endif%>**<%=(doc:module-documentation env)%>
<%@endloop%><%@endif%>")

(defclass system-documentation (document id)
  ((system :initarg :system :accessor doc-object :type system)
   (packages :initarg :packages :accessor doc-packages :type (vector package-documentation))))

(defmethod print-object ((self system-documentation) stream)
  (print-unreadable-object (self stream :type t)
    (let ((sys (slot-value self 'system)))
      (format stream "~A ~A" (name sys) (version sys)))))

(defmethod description ((self system-documentation)) (description (doc-object self)))
(defmethod name ((self system-documentation)) (name (doc-object self)))
(defmethod version ((self system-documentation)) (version (doc-object self)))
(defmethod components ((self system-documentation)) (components (doc-object self)))
(defmethod module-provide ((self system-documentation)) (module-provide (doc-object self)))
(defmethod module-require ((self system-documentation)) (module-require (doc-object self)))
(defmethod path ((self system-documentation)) (path (doc-object self)))

(defun system-documentation (sys &optional packages) 
  (unless (typep sys 'system) (setf sys (find-system sys)))
  (make-instance 'system-documentation 
    :id (sxhash sys)
    :system sys
    :packages 
    (or packages 
        (collecting
          (mapc (lambda (x) 
                  (when (string-prefix-p (name sys) (package-name x))
                    (ignore-errors
                     (when-let ((pkg (package-documentation x :external)))
                       (collect pkg)))))
                (list-all-packages))))))

(defmethod change-class ((self system) (new (eql 'system-documentation)) &key packages)
  (system-documentation self packages))

(defmethod dependents ((self system-documentation))
  (mapcar #'system-documentation (find-system-dependents (doc-object self))))

(defmethod dependencies ((self system-documentation))
  (mapcar #'system-documentation (component-require (doc-object self))))

(defmethod doc-files ((self system-documentation))
  "Return a list of source file components from SELF."
  (when-let ((sys (doc-object self)))
    (cons (path sys)
          (when-let ((comp (components sys)))
            (flet ((%rec (s) (if (typep s 'mod-component)
                                 (doc-files s)
                                 (when s
                                   (path s)))))
              (flatten (mapcar #'%rec comp)))))))

(defmethod doc-files ((self mod-component))
  (labels ((%rec (s) (if (typep s 'mod-component)
                         (doc-files s)
                         (when s (path s)))))
    (mapcar #'%rec (components self))))

(defun protocol-documentation (form &optional (module *document-module*)))

(defun module-documentation (form &optional (module *document-module*))
  "Return a simple org-heading describing the module designated by FORM as a
string."
  (with-output-to-string (s)
    (destructuring-bind (key &rest args) form
      (let ((name))
        (case key
          (:tests 
           (format s "* ~A :~A:~%" (if module (concatenate 'string (string module) "/" (string key)) key) key)
           ;; (find-module module key)
           )
          (:prelude
           (format s "* ~A :~A:~%" (setf name (pop args)) key)
           (format s "- Exports~%~{  - ~A~%~}" 
                   (mapcar #'find-symbol-normalize args)))
          (:proto
           (format s "* ~A :~A:~%" (setf name (pop args)) key)
           (doplist (k v) args
             (when v
               (format s "- ~A~%~<  - ~;~A~>~%" 
                       k v))))
          (t
           (format s "* ~A :~A:~%" (setf name (pop args)) key)
           (doplist (k v) args
             (when v
               (format s "- ~A~%  #+begin_src lisp-data :eval no~%~<  ~;~S~>~%  #+end_src~%" 
                       k v)))))))))

(defmethod documentation ((object list) (doc-type (eql 'module)))
  (module-documentation object))

;; TODO 2026-06-27: export-file-name?
(defmethod publish ((self system-documentation) &key output info level (file-name-p *document-multi-file*) (prune *document-prune*))
  (with-slots (id packages) self
    (let* ((*document-module* (name self))
           (file (file-documentation (path self)))
           (gen (execute-template (keywordicate (class-name (class-of self)))
                                  :env
                                  `(:name ,(name self) :id ,id
                                    :location ,(if *document-project-name*
                                                   (with-output-to-string (s)
                                                     (fmt-vc-link s
                                                                  *document-project-name*
                                                                  #1=(enough-namestring (path self))
                                                                  #1#))
                                                   (enough-namestring (path self)))
                                    :summary ,(file-summary file)
                                    :level ,level
                                    :info ,info
                                    :commentary ,(file-commentary file)
                                    :description ,(description self)
                                    ,@(when file-name-p
                                        `(:export-file-name ,(format nil "api/~(~A~)" (name self))))
                                    :file-description ,(file-description file)
                                    :components ,(components self)
                                    :version ,(version self)
                                    :provide ,(module-provide self)
                                    :require ,(module-require self)
                                    ;; :tags ,(file-tag-string self)
                                    :packages ,(if prune
                                                   (remove-if 
                                                    (lambda (x) (null (ast x))) 
                                                    packages)
                                                   packages)))))
      (case output
        ('nil gen)
        (:string gen)
        (t (if (pathnamep output)
               (with-output-to-file (f output)
                 (write-string gen f))
               (write-string gen output)))))))
