;;; lib/dat/sxp.lisp --- S-eXPressions

;; A portable S-Expression data format

;;; Code:
(in-package :dat/sxp)

;;; Conditions
(define-condition sxp-error (error) ())

(define-condition sxp-syntax-error (sxp-error) ())

 ;;; Protocol
(defgeneric sxpp (self form))

(defgeneric write-sxp-stream (self stream &key pretty case))
(defgeneric read-sxp-stream (self stream))

;;; Objects
(defmethod write-sxp-stream ((self ast) stream &key (pretty *print-pretty*) (case :downcase))
  (write (ast self)
	 :stream stream
	 :pretty pretty
	 :case case))

(defmethod read-sxp-stream ((self ast) stream)
  (setf (ast self) (slurp-stream-forms stream :count nil)))

;; (defsetf unwrap ) (defsetf wrap )

;;; Functions
(defun read-sxp-file (file)
  (make-instance 'ast :ast (read-file-forms file)))

(defun write-sxp-file (sxp file &optional &key if-exists)
  (with-output-file (out file) :if-exists if-exists
    (write-sxp-stream sxp out)))

(defun read-sxp-string (self str) (with-input-from-string (s str) (read-sxp-stream self s)))

(defun write-sxp-string (sxp) 
  (let ((ast (ast sxp)))
    (declare (list ast))
    (if (> (length ast) 1)
	(write-to-string ast)
	(write-to-string (car ast)))))

(defun make-sxp (&rest form) (make-instance 'ast :ast form))

(deftype sxp-fmt-designator () '(member :canonical :collapsed :pretty)) 

(defun file-read-forms (file)
  (declare (pathname-designator file))
  (awhen (the list (read-file-forms file))
    (if (> (length it) 1)
        it
        (car it))))
