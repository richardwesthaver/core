;;; web/index.lisp --- local user index

;;; Code:
(uiop:define-package :web/index
    (:use :cl :std :hunchentoot :lass :spinneret)
  (:export 
   :main
   :*web-index-port*))

(in-package :web/index)

(defparameter *last-update* (get-universal-time))

(defun current-time () (setq *last-update* (get-universal-time)))

(defparameter *web-index-port* 8888)

(defparameter *server* 
  (make-instance 'easy-acceptor 
    :port 8888
    :name "index"))

(define-easy-handler (b :uri "/b") (user)
  (setf (content-type*) "text/plain")
  (format nil "showing buffers for ~@[ ~A~]." user))

(define-easy-handler (i :uri "/i") (user)
  (setf (content-type*) "text/plain")
  (format nil "showing inbox for ~@[ ~A~]." user))

(define-easy-handler (a :uri "/a") (user)
  (setf (content-type*) "text/plain")
  (format nil "showing agenda for ~@[ ~A~]." user))

(define-easy-handler (org :uri "/org") (user)
  (setf (content-type*) "text/plain")
  (format nil "showing org-files for ~@[ ~A~]." user))

(deftag link (link body)
  `(:a :href ,@link ,@body))

(defmacro with-index-page (&optional (title "local index") &body body)
    `(with-html 
       (:doctype)
       (:html
        (:head
         (:title ,title)
         (:body 
          (:div :class "nav"
           "( "
           (link "https://compiler.company" "~")
           (link "https://compiler.company/blog" "blog")
           (link "https://compiler.company/docs" "docs")
           (link "https://compiler.company/code" "code")
           " )")
          ,@body
          (:footer ("Last update: ~A" (current-time))))))))

 (defun tabulate (&rest rows)
   (with-html
     (flet ((tabulate ()
              (loop for row in rows do
                (:tr (loop for cell in row do
                  (:td cell))))))
       (if (find :table (get-html-path))
           (tabulate)
           (:table (:tbody (tabulate)))))))

(defun inner-section ()
  "Binds *HTML-PATH* to replicate the depth the output is used in."
  (with-html-string
    (let ((*html-path* (append *html-path* '(:section :section))))
      (:h* "Heading three levels deep"))))

(defun outer-section (html)
  "Uses HTML from elsewhere and embed it into a section"
  (with-html-string
    (:section
     (:h* "Heading two levels deep")
     (:section
      (:raw html)))))

(defun main (&key (output *standard-output*) (port *web-index-port*))
  (let ((*standard-output* output))
    (print "starting index server on ~A" port)
    (start *server*)))

(defun shutdown (&optional (target *server*))
  (stop target))
