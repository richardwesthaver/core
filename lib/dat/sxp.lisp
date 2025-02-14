;;; lib/dat/sxp.lisp --- S-eXPressions

;; A portable S-Expression data format

;;; Code:
(in-package :dat/sxp)

;;; Conditions

 ;;; Protocol
(defgeneric sxpp (self form))

;;; Objects
(defmethod write-ast ((self ast:ast) stream &key (pretty *print-pretty*) (case :downcase))
  (write (ast:ast self)
	 :stream stream
	 :pretty pretty
	 :case case))

(defmethod read-ast ((self ast:ast) stream &key)
  (setf (ast:ast self) (slurp-stream-forms stream :count nil)))

;; (defsetf unwrap ) (defsetf wrap )

;;; Functions
(defun read-sxp-file (file)
  (make-instance 'ast:ast :ast (read-file-forms file)))

(defun write-sxp-file (sxp file &optional &key if-exists)
  (with-output-file (out file) :if-exists if-exists
    (write-ast sxp out)))

(defun read-sxp-string (self str) (with-input-from-string (s str) (read-ast self s)))

(defun write-sxp-string (sxp) 
  (let ((ast (ast:ast sxp)))
    (declare (list ast))
    (if (> (length ast) 1)
	(write-to-string ast)
	(write-to-string (car ast)))))

(defun make-sxp (&rest form) (make-instance 'ast:ast :ast form))

(deftype sxp-fmt-designator () '(member :canonical :collapsed :pretty))

(defun file-read-forms (file)
  (declare (sb-kernel:pathname-designator file))
  (awhen (the list (read-file-forms file))
    (if (> (length it) 1)
        it
        (car it))))
