;;; web/index.lisp --- local user index

;;; Code:
(uiop:define-package :app/web/index
    (:use :cl :std :hunchentoot :lass :spinneret)
  (:export 
   :main
   :*web-index-port*))

(in-package :app/web/index)

(defparameter *last-update* (get-universal-time))

(defun current-time () (setq *last-update* (get-universal-time)))

(defparameter *web-index-port* 8888)

(defmacro with-index-page (&optional (title "local index") &body body)
    `(with-html 
       (:doctype)
       (:html
        (:head
         (:title ,title)
         (:body 
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

(defun main (&key (output *standard-output*))
  (let ((*standard-output* output))
    (print "starting index server on ~A" *web-index-port*)))
