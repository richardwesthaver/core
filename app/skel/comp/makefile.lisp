;;; makefile.lisp --- GNU Makefile Components

;; GNU Makefile skel components.

;;; Commentary:

;; Makefiles are a reasonably portable build medium. We can parse them using
;; the same general strategy as GNU make and compile them from skelfiles
;; (rule, source, target, command).

;;  HACK 2023-09-15: MVP

;; SO, the absolute priority ATM is to transpile our `sk-rule' objects
;; into a working Makefile. We're ignoring most of the niceties like
;; line-splitting and any JIT or compile-time execution.

;; https://github.com/takagi/lake

;; https://www.gnu.org/software/make/manual/html_node/Parsing-Makefiles.html

;;; Code:
(in-package :skel/comp/makefile)

(defparameter *default-makefile* "makefile")
(defparameter *makefile-extension* "mk")

;;  TODO 2023-09-27: what is $(@D) ?? (target-dir)
(defvar *mk-magic-vars* #(#\@ #\< #\^ #\* #\+ #\? #\|))

(defvar *mk-command-prefixes* #(#\@ #\- #\+))

(deftype mk-val-designator () '(member nil :simple :immediate :conditional :recursive :once :append :shell))

(defstruct mk-val (kind nil :type mk-val-designator)  (val nil :type ast:form))

(defstruct mk-var
  (key "" :type string)
  (val (make-mk-val) :type mk-val))

;; https://www.gnu.org/software/make/manual/html_node/Makefile-Contents.html
(defclass makefile (skel sk-meta)
  ((directives :initform (make-array 0 :adjustable t :fill-pointer 0)
	       :type (vector list) :accessor mk-directives)
   (variables :initform (make-hash-table)
	      :type (hash-table) :accessor mk-vars)
   (explicit :initform (make-array 0 :element-type 'sk-rule :adjustable t :fill-pointer 0)
	     :type (vector sk-rule) :accessor mk-erules)
   (implicit :initform (make-array 0 :element-type 'sk-rule :adjustable t :fill-pointer 0) 
	     :type (vector sk-rule) :accessor mk-irules))
  (:documentation "A virtual GNU Makefile."))

(defmethod push-mk-rule ((self sk-rule) (place makefile) &optional implicit)
  (if implicit
      (vector-push-extend self (mk-irules place))
      (vector-push-extend self (mk-erules place))))

(defmethod push-mk-directive ((self list) (place makefile))
  (vector-push-extend self (mk-directives place)))

(defmethod push-mk-var ((self cons) (place makefile))
  (destructuring-bind (k v) self
    (setf (gethash k (mk-vars place)) v)))

(defmethod sk-compile ((self makefile) &key stream &allow-other-keys)
  "Compile the makefile SELF to output STREAM."
  (with-open-stream (s stream)
    (with-slots (directives variables explicit implicit) self
      ;; directives
      (loop for d across directives
	    do (write d :stream s)
            do (terpri s))
      ;; variables
      (maphash (lambda (x y) (format s "~A=~A~%" x y)) variables)
      ;; explicit rules
      (loop for exp across explicit
	    do (format s "~A:~A;~A~%" 
		       (sk-rule-target exp)
		       (sk-rule-source exp)
                       (when-let ((recipe (sk-rule-recipe exp)))
		         (sk-write-string recipe))))
      ;; TODO implicit rules
      (loop for imp across implicit
	    do (format s "~A:~A;~A~%" 
		       (sk-rule-target imp)
		       (sk-rule-source imp)
		       (sk-write-string (sk-rule-recipe imp)))))))

(defmethod sk-write-file ((self makefile) &key (path *default-makefile*) (comment t) (if-exists :overwrite))
  (with-open-file (out path
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
    (when comment (princ
		   (make-source-header-comment
		    (name self)
		    :cchar #\#
		    :timestamp t
		    :description (sk-description self)
		    :opts '("mode: makefile-gmake;"))
		   out))
    (sk-compile self :stream out)))

(defmethod sk-read-file ((self makefile) path)
  (with-open-file (f path :direction :input)))

;;; Auto Vars

;; simplified version of GNU Make Automatic Variables

;; don't need these: $% $? $+ $*

;; (defmacro def-mk-auto (sym ll &body body))

;; (def-mk-auto $@ (rule) (sk-rule-target rule))
;; (def-mk-auto $< (rule) (car (sk-rule-source rule)))
;; (def-mk-auto $^ (rule) (sk-rule-source rule))

