;;; system.lisp --- Lisp System Documentation

;; Standard System Documentation.

;;; Commentary:

;; This module provides the SYSTEM-DOCUMENTATION class which wraps a
;; STD:SYSTEM and provides a basic documentation-focused API.

;; SYSTEM-DOCUMENTATION is the most high-level documentation class provided
;; and is intended to be encoded into a tree of ORG-DOCUMENT objects.

;;; Code:
(in-package :doc)

(deftempo :system-documentation
  "<%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%> <%@var name%>
:PROPERTIES:
<%@ifnotempty summary%>:SUMMARY: <%@var summary%>
<%@endif%><%@ifnotempty file-description%>:DESCRIPTION: <%@var file-description%>
<%@endif%><%@ifnotempty version%>:VERSION: <%@var version%>
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
<%@loop packages%><%=(doc:publish env :output :string :level 3)%>
<%@endloop%><%@endif%><%@ifnotempty components%>
<%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%>* Components
<%@loop components%><%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%>*<%=(doc:publish env :output :string :level 2)%>
<%@endloop%><%@endif%><%@ifnotempty provide%>** Modules
<%@loop provide%>- todo
<%@endloop%><%@endif%>")

(defclass system-documentation (document id)
  ((system :initarg :system :accessor doc-object :type system)
   (packages :initarg :packages :accessor doc-packages :type (vector package-documentation))))

(defmethod print-object ((self system-documentation) stream)
  (print-unreadable-object (self stream :type t)
    (let ((sys (slot-value self 'system)))
      (format stream "~A ~A" (name sys) (version sys)))))

(defaccessor description ((self system-documentation)) (description (doc-object self)))
(defaccessor name ((self system-documentation)) (name (doc-object self)))
(defaccessor version ((self system-documentation)) (version (doc-object self)))
(defaccessor components ((self system-documentation)) (components (doc-object self)))
(defaccessor module-provide ((self system-documentation)) (module-provide (doc-object self)))
(defaccessor module-require ((self system-documentation)) (module-require (doc-object self)))
(defmethod path ((self system-documentation)) (path (doc-object self)))

(defun system-documentation (sys &optional packages) 
  (unless (typep sys 'system) (setf sys (find-system sys)))
  (make-instance 'system-documentation 
    :id (make-v5-uuid +namespace-oid+ (format nil "SYSTEM:~A" (name sys)))
    :system sys
    :packages (or packages 
                  (collecting
                    (mapc (lambda (x) (when (string-prefix-p (name sys) (package-name x))
                                        (ignore-errors
                                         (collect (package-documentation x)))))
                          (list-all-packages))))))

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

(defmethod publish ((self system-documentation) &key output info)
  (with-slots (id name packages) self
    (let* ((file (file-documentation (path self)))
           (gen (execute-template (keywordicate (class-name (class-of self)))
                                 :env
                                 `(:name ,(name self) :id ,id
                                   :location ,(enough-namestring (path self))
                                   :summary ,(file-summary file)
                                   :info ,info
                                   :commentary ,(file-commentary file)
                                   :description ,(description self)
                                   :file-description ,(file-description file)
                                   :components ,(components self)
                                   :version ,(version self)
                                   :provide ,(module-provide self)
                                   ;; :require ,(module-require self)
                                   ;; :tags ,(file-tag-string self)
                                   :packages ,packages))))
      (case output
        ('nil (values (org-parse (document-keyword self) gen) gen))
        (:string gen)
        (t (write-string gen output))))))
