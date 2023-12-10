;;; makefile.lisp --- GNU Makefile compiler

;; GNU Makefile compiler.

;;; Commentary:

;; Makefiles are our 'portable' build medium. We can parse them using
;; the same general strategy as GNU make and compile them from
;; skelfiles (rule, source, target, command).

;;  HACK 2023-09-15: MVP

;; SO, the absolute priority ATM is to transpile our `sk-rule' objects
;; into a working Makefile. We're ignoring most of the niceties like
;; line-splitting and any JIT or compile-time execution.

;; https://github.com/takagi/lake

;; https://www.gnu.org/software/make/manual/html_node/Parsing-Makefiles.html

;;; Code:
(in-package :skel/comp)

(defparameter *default-makefile* "makefile")
(defparameter *makefile-extension* "mk")

;;  TODO 2023-09-27: what is $(@D) ?? (target-dir)
(defvar *mk-magic-vars* #(#\@ #\< #\^ #\* #\+ #\? #\|))

(defvar *mk-command-prefixes* #(#\@ #\- #\+))

(deftype mk-val-designator () '(member nil :simple :immediate :conditional :recursive :once :append :shell))

(defstruct mk-val "" (kind nil :type mk-val-designator)  (val nil :type form))

(defstruct mk-var ""
	   (key "" :type string)
	   (val (make-mk-val) :type mk-val))

;; https://www.gnu.org/software/make/manual/html_node/Makefile-Contents.html
(defclass makefile (skel sk-meta)
  ((directives :initform (make-array 0 :element-type 'sk-command :adjustable t :fill-pointer 0) 
	       :type (vector sk-command) :accessor mk-directives)
   (variables :initform (make-hash-table) 
	      :type (hash-table) :accessor mk-vars)
   (explicit :initform (make-array 0 :element-type 'sk-rule :adjustable t :fill-pointer 0)
	     :type (vector sk-rule) :accessor mk-erules)
   (implicit :initform (make-array 0 :element-type 'sk-rule :adjustable t :fill-pointer 0) 
	     :type (vector sk-rule) :accessor mk-irules))
  (:documentation "A virtual GNU Makefile."))

(defmethod push-rule ((self sk-rule) (place makefile) &optional implicit)
  (if implicit
      (vector-push-extend self (mk-irules place))
      (vector-push-extend self (mk-erules place))))

(defmethod push-directive ((self sk-command) (place makefile))
  (vector-push-extend self (mk-directives place)))

(defmethod push-var ((self cons) (place makefile))
  (destructuring-bind (k v) self
    (setf (gethash k (mk-vars place)) v)))

(defmethod sk-compile ((self makefile) stream &key &allow-other-keys)
  "Compile the makefile SELF to output STREAM."
  (with-open-stream (s stream)
    (with-slots (directives variables explicit implicit) self
      ;; directives
      (loop for d across directives
	    do (sk-writeln d s))
      ;; variables
      (maphash (lambda (x y) (format s "~A=~A~%" x y)) variables)
      ;; explicit rules
      (loop for exp across explicit
	    do (format s "~A:~A;~A~%" 
		       (sk-write-string (sk-rule-target exp))
		       (sk-write-string (sk-rule-source exp))
		       (sk-write-string (sk-rule-recipe exp))))
      ;; TODO implicit rules
      (loop for imp across implicit
	    do (format s "~A:~A;~A~%" 
		       (sk-write-string (sk-rule-target imp))
		       (sk-write-string (sk-rule-source imp))
		       (sk-write-string (sk-rule-recipe imp)))))))

(defmethod sk-write-file ((self makefile) &key (path *default-makefile*) (comment t) (if-exists :overwrite))
  (with-open-file (out path
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
    (when comment (princ
		   (make-source-header-comment
		    (sk-name self)
		    :cchar #\#
		    :timestamp t
		    :description (sk-description self)
		    :opts '("mode: makefile-gmake;"))
		   out))
    (sk-compile self out)))

(defmethod sk-read-file ((self makefile) path))
