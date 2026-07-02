;;; lib/doc/project.lisp --- Project Documentation

;; Document an entire project.

;;; Commentary:

;;

;;; Code:
(in-package :doc)

(deftempo :project-documentation
  "#+TITLE: <%@var name%><%@ifnotempty summary%>
#+SUBTITLE: <%@var summary%><%@endif%><%@ifnotempty file-description%>
#+DESCRIPTION: <%@var file-description%><%@endif%><%@if version%>
#+VERSION: <%@var version%><%@endif%><%@if id%>
#+ID: <%@var id%><%@endif%><%@ifnotempty location%>
#+LOCATION: <%@var location%><%@endif%><%@ifnotempty author%>
#+AUTHOR: <%@var author%><%@endif%><%@if email%>
#+EMAIL: <%@var email%><%@endif%><%@ifnotempty tags%>
#+FILETAGS:<%@loop tags%> <%=env%><%@endloop%><%@endif%><%@ifnotempty setupfile%>
#+SETUPFILE: <%@var setupfile%><%@endif%>
#+TODO:
<%@var description%>
<%@if info%>
#+BEGIN: project-info
#+END:
<%@endif%><%@if readme%>
<%@var readme%>
<%@endif%><%@if commentary%>
<@var commentary%>
<%@endif%><%@ifnotempty rules%>
:rules:
<%@loop rules%><%=(doc:publish env)%>
<%@endloop%>:end:
<%@endif%><%@ifnotempty systems%>
* Systems<%@loop systems%>
<%=(doc:publish env :output :string :level 2)%><%@endloop%><%@endif%>")

(defmethod publish ((self file-component) &rest args)
  (apply 'publish (change-class self 'file-documentation) args))

(defmethod publish ((self mod-component) &rest args)
  (apply 'publish (change-class self 'mod-documentation) args))

(deffmt fmt-rule "- ~A~@[ (~{~(~A~)~^ ~})~]~@[ \\\\~%~A~]")

(deffmt fmt-vc-url "[[vc:~A][~A]]")

(defmethod publish ((self simple-rule) &key)
  (with-output-to-string (s)
    (fmt-rule s (rule-target self) (source self) (kernel-documentation self))))

(defclass project-documentation (document)
  ((project :initarg :project :accessor doc-object :type project)))

(defun project-documentation (&optional (project *project*) systems include-test-systems)
  "Return the documentation instance of project S."
  (unless (typep project 'project) (setf project (find-project project)))
  (make-instance 'project-documentation
    :id (id project)
    :project project
    :ast (mapcar (lambda (x) (change-class x 'system-documentation))
                 (or systems (directory-systems (path project) include-test-systems)))))

(defmethod print-object ((self project-documentation) stream)
  (print-unreadable-object (self stream :type t)
    (let ((proj (slot-value self 'project)))
      (format stream "~A ~A" (name proj) (version proj)))))

(defmethod dependents ((self project-documentation))
  (mapcar #'system-documentation (find-system-dependents (doc-object self))))

(defmethod dependencies ((self project-documentation))
  (mapcar #'system-documentation (component-require (doc-object self))))

(defmethod id ((self project-documentation)) (id (doc-object self)))
(defaccessor name ((self project-documentation)) (name (doc-object self)))
(defaccessor version ((self project-documentation)) (version (doc-object self)))
(defaccessor description ((self project-documentation)) (description (doc-object self)))
(defaccessor links ((self project-documentation)) (links (doc-object self)))
(defaccessor module-provide ((self project-documentation)) (slot-boundp (doc-object self) 'provide))
(defaccessor module-require ((self project-documentation)) (module-require (doc-object self)))
(defaccessor rules ((self project-documentation)) (rules (doc-object self)))
(defaccessor author ((self project-documentation)) (author (doc-object self)))

(defmethod path ((self project-documentation))
  (path (doc-object self)))

(defmethod publish ((self project-documentation) &key output info setupfile vc (local t))
  (with-slots (project ast) self
    (let* ((*default-pathname-defaults* (path self))
           (*document-project-name* (name project))
           (file (file-documentation (path self)))
           (author (if (consp #1=(author self)) (car #1#) #1#))
           (email (when (consp #1#) (cdr #1#)))
           (tags (tags project))
           (vc (or vc (uri (vc-remote (vc project) 'default))))
           (gen (execute-template (keywordicate (class-name (class-of self)))
                                  :env
                                  `(:name ,(name self) :id ,(id self)
                                    :location ,(if local (namestring (project-root self)) vc)
                                    :summary ,(file-summary file)
                                    :info ,info
                                    :commentary ,(file-commentary file)
                                    :description ,(description self)
                                    :file-description ,(file-description file)
                                    ;; :components ,(coerce (components self) 'list)
                                    :setupfile ,setupfile
                                    :version ,(version self)
                                    :provide ,(module-provide self)
                                    :rules ,(coerce (rules self) 'list)
                                    :author ,author
                                    :email ,email
                                    ;; :require ,(module-require self)
                                    :tags ,tags
                                    :systems ,ast))))
      (case output
        ('nil gen)
        (:string gen)
        (t 
         (if (pathnamep output)
             (with-output-to-file (f output) (write-string gen f))
             (write-string gen output)))))))
