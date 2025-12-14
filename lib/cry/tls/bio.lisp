;;; ctx.lisp --- SSL BIO Methods

;; 

;;; Code:
(in-package :cry/tls)

;;; BIO
(defparameter *bio-blockp* t)
(defvar *bio-socket*)
(defvar *lisp-bio-type*)
(defvar *bio-lisp-method* nil)

(defun lisp-bio-type ()
  (logior (openssl::bio-get-new-index) openssl::+BIO-TYPE-SOURCE-SINK+))

(defun bio-init ()
  (setf *lisp-bio-type* (lisp-bio-type)
        *bio-lisp-method* (make-bio-lisp-method)))

#+nil
(defun make-bio-lisp-method-slots ()
  (let ((m (make-alien openssl::bio-method)))
    (setf (slot m 'openssl::type) *lisp-bio-type*
          (slot m 'openssl::name) (make-alien-string "lisp")
          (slot m 'openssl::bwrite) (alien-callable-function 'lisp-write)
          (slot m 'openssl::bread) (alien-callable-function 'lisp-read)
          (slot m 'openssl::bputs) (alien-callable-function 'lisp-puts)
          (slot m 'openssl::bgets) (alien-callable-function 'lisp-gets)
          (slot m 'openssl::ctrl) (alien-callable-function 'lisp-ctrl)
          (slot m 'openssl::create) (alien-callable-function 'lisp-create-slots)
          (slot m 'openssl::destroy) (alien-callable-function 'lisp-destroy-slots)
          (slot m 'openssl::callback-ctrl) nil)
    m))

(defun make-bio-lisp-method ()
  (let ((m (openssl::bio-meth-new *lisp-bio-type* "lisp")))
    (openssl::bio-meth-set-puts m (alien-sap (alien-callable-function 'lisp-puts)))
    (openssl::bio-meth-set-write m (alien-sap (alien-callable-function 'lisp-write)))
    (openssl::bio-meth-set-read m (alien-sap (alien-callable-function 'lisp-read)))
    (openssl::bio-meth-set-gets m (alien-sap (alien-callable-function 'lisp-gets)))
    (openssl::bio-meth-set-create m (alien-sap (alien-callable-function 'lisp-create)))
    (openssl::bio-meth-set-destroy m (alien-sap (alien-callable-function 'lisp-destroy)))
    (openssl::bio-meth-set-ctrl m (alien-sap (alien-callable-function 'lisp-ctrl)))
    m))

(defun bio-new-lisp ()
  (unless *bio-lisp-method* (bio-init))
  (let ((new (openssl::bio-new *bio-lisp-method*)))
    (if (or (null new) (null-alien new))
        (error "Cannot create bio method: ~a"
               (openssl::err-error-string (openssl::err-get-error) (make-alien char openssl::+err-error-string-buf-len+)))
        new)))

(defun clear-retry-flags (bio)
  (openssl::bio-clear-flags 
   bio
   #.(logior openssl::+BIO-FLAGS-RWS+
             openssl::+BIO-FLAGS-SHOULD-RETRY+)))

(defun set-retry-read (bio)
  (openssl::bio-set-flags 
   bio
   #.(logior openssl::+BIO-FLAGS-READ+
             openssl::+BIO-FLAGS-SHOULD-RETRY+)))

(define-alien-callable lisp-write int
    ((bio (* openssl::bio))
     (buf (* char))
     (n int))
  (dotimes (i n)
    (write-byte (deref buf i) *bio-socket*))
  (finish-output *bio-socket*)
  n)
  
(define-alien-callable lisp-read int
    ((bio (* t))
     (buf (* unsigned-char))
     (n int))
        (let ((i 0))
          (handler-case
              (progn
                (clear-retry-flags bio)
                (loop
                  while (and (< i n)
                             (or *bio-blockp* (listen *bio-socket*)))
                  do (setf (deref buf i) (read-byte *bio-socket*))
                     (incf i))
                (when (zerop i) (set-retry-read bio)))
            (end-of-file ()
              (openssl::bio-set-flags bio #.openssl::+bio-flags-in-eof+)
              ;; now just return the number of bytes read so far
              ))
        ;; Old OpenSSL treats zero as EOF and signals an error:
        ;; "The TLS/SSL connection on handle #<A Foreign Pointer #x7F42DC082880> has been closed (return code: 5)"
        ;; despite our implementation of (BIO_ctrl ... +BIO_CTRL_EOF+)
        ;; returns false.
        ;; (This was observed on openssl-1.1.0j. And
        ;; on OpenSSL 3 it does not happen).
        ;; Since both 0 and -1 are allowed by the docs,
        ;; let's return -1 instead of 0.
          (if (= 0 i) -1 i)))

(define-alien-callable lisp-puts int
    ((bio (* t))
     (buf c-string))
  (write-line buf (make-instance 'wrapped-character-output-stream
                    :stream *bio-socket*))
  ;; puts is not specified to return length, but BIO expects it :(
  (1+ (length buf)))
  
(define-alien-callable lisp-gets int ((bio (* t)) (buf (* char)) (n int))
  (let ((i 0)
        (max-chars (1- n)))
    (clear-retry-flags bio)
    (handler-case
        (loop
          with char
          and exit = nil
          while (and (< i max-chars)
                     (null exit)
                     (or *bio-blockp* (listen *bio-socket*)))
          do
             (setf char (read-byte *bio-socket*)
                   exit (= char 10))
             (setf (deref buf i) char)
             (incf i))
      (end-of-file ()
        (bio-set-flags bio openssl::+BIO-FLAGS-IN-EOF+)))
    (setf (deref buf i) 0)
    i))

(define-alien-callable lisp-ctrl int
    ((bio (* t))
     (cmd int)
     (larg long)
     (parg (* t)))
  (cond
    ((eql cmd openssl::+bio-ctrl-eof+)
     (if (zerop (openssl::bio-test-flags bio openssl::+BIO-FLAGS-IN-EOF+))
         0 1))
    ((eql cmd openssl::+BIO-CTRL-FLUSH+) 1)
    (t 0)))

(define-alien-callable lisp-create int
    ((bio (* t)))
  (openssl::bio-set-init bio 1)
  (clear-retry-flags bio)
  1)

(define-alien-callable lisp-destroy int
    ((bio (* t)))
  (cond
    ((null-alien bio) 0)
    (t
     (openssl::bio-set-init bio 0)
     (clear-retry-flags bio)
     1)))

(defmacro with-bio-output-to-string ((bio &key (element-type ''character)
                                               (transformer '#'code-char))
                                     &body body)
  "Evaluate BODY with BIO bound to a SSL BIO structure that writes to a
Common Lisp string.  The string is returned."
  `(let ((*bio-socket* (make-in-memory-output-stream 
                        :element-type ,element-type
                        :transformer ,transformer))
         (,bio (bio-new-lisp)))
     (unwind-protect
          (progn ,@body)
       (bio-free ,bio))
     (get-output-stream-sequence *bio-socket*)))

(defmacro with-bio-input-from-string ((bio
                                       string
                                       &key (transformer '#'char-code))
                                      &body body)
  "Evaluate BODY with BIO bound to a SSL BIO structure that reads from
a Common Lisp STRING."
  `(let ((*bio-socket* (make-in-memory-input-stream ,string :transformer ,transformer))
         (,bio (bio-new-lisp)))
     (unwind-protect
          (progn ,@body)
       (bio-free ,bio))))
