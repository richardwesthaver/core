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
#+FILETAGS: <%@var tags%><%@endif%>
<%@var description%>
<%@if info%>
#+BEGIN: project-info
#+END:
<%@endif%><%@if readme%>
<%@var readme%>
<%@endif%><%@if commentary%>
<@var commentary%>
<%@endif%><%@ifnotempty components%>
* Components
<%@loop components%>

<%@endloop%>
<%@endif%><%@ifnotempty rules%>
* Rules
<%@loop rules%>

<%@endloop%>
<%@endif%>")

(defclass project-documentation (document id)
  ((project :initarg :project :accessor doc-object :type project)
   ;; TODO 2026-06-20: component-documentation
   (systems :initarg :systems :accessor doc-systems :type (vector system-documentation))))

(defun project-documentation (&optional (project *project*) systems)
  "Return the documentation instance of project S."
  (unless (typep project 'project) (setf project (find-project project)))
  (make-instance 'project-documentation
    :id (make-v5-uuid +namespace-oid+ (format nil "PROJECT:~A" (name project)))
    :project project
    :systems systems))

(defmethod print-object ((self project-documentation) stream)
  (print-unreadable-object (self stream :type t)
    (let ((proj (slot-value self 'project)))
      (format stream "~A ~A" (name proj) (version proj)))))
    
(defmethod dependents ((self project-documentation))
  (mapcar #'system-documentation (find-system-dependents (doc-object self))))

(defmethod dependencies ((self project-documentation))
  (mapcar #'system-documentation (component-require (doc-object self))))

;; (defaccessor id ((self project-documentation)) (id (doc-object self)))
(defaccessor name ((self project-documentation)) (name (doc-object self)))
(defaccessor version ((self project-documentation)) (version (doc-object self)))
(defaccessor description ((self project-documentation)) (description (doc-object self)))
(defaccessor links ((self project-documentation)) (links (doc-object self)))
(defaccessor components ((self project-documentation)) (components (doc-object self)))
(defaccessor module-provide ((self project-documentation)) (slot-boundp (doc-object self) 'provide))
(defaccessor module-require ((self project-documentation)) (module-require (doc-object self)))
(defaccessor rules ((self project-documentation)) (rules (doc-object self)))
(defaccessor author ((self project-documentation)) (author (doc-object self)))

(defmethod path ((self project-documentation))
  (path (doc-object self)))

(defmethod publish ((self project-documentation) &key output info)
  (with-slots (project systems) self
    (let* ((file (file-documentation (path self)))
           (author (if (consp #1=(author self)) (car #1#) #1#))
           (email (when (consp #1#) (cdr #1#)))
           (gen (execute-template (keywordicate (class-name (class-of self)))
                                 :env
                                 `(:name ,(name self) :id ,(id self)
                                   :location ,(enough-namestring (path self))
                                   :summary ,(file-summary file)
                                   :info ,info
                                   :commentary ,(file-commentary file)
                                   :description ,(description self)
                                   :file-description ,(file-description file)
                                   :components ,(coerce (components self) 'list)
                                   :version ,(version self)
                                   :provide ,(module-provide self)
                                   :rules ,(coerce (rules self) 'list)
                                   :author ,author
                                   :email ,email
                                   ;; :require ,(module-require self)
                                   ;; :tags ,(file-tag-string self)
                                   :systems ,systems))))
      (case output
        ('nil (values (org-parse (document-keyword self) gen) gen))
        (:string gen)
        (t (write-string gen output))))))
