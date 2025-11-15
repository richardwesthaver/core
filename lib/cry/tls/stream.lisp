;;; stream.lisp --- SSL Streams

;; 

;;; Code:
(in-package :tls)

(defclass* ssl-stream (wrapped-stream
                      fundamental-binary-input-stream
                      fundamental-binary-output-stream)
  (socket close-callback
   (sap :initform nil :accessor sap)
   (deadline :initform nil)
   (output-buffer :accessor output-buffer)
   (output :accessor output :initform 0)
   (input-buffer :accessor input-buffer)
   (peeked :accessor peeked :initform nil)))

(defmethod initialize-instance :after ((self ssl-stream)
                                       &key (buffer-size *ssl-buffer-size*)
                                       (input-buffer-size buffer-size)
                                       (output-buffer-size buffer-size)
                                       &allow-other-keys)
  (setf (output-buffer self) (make-static-vector output-buffer-size)
        (input-buffer self) (make-static-vector input-buffer-size)))

(defmethod print-object ((self ssl-stream) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "for ~A" (ssl-stream-socket self))))

(defmethod stream-element-type ((stream ssl-stream)) '(unsigned-byte 8))

(defmethod close ((self ssl-stream) &key abort)
  (cond
    ((sap self)
     (unless abort
       (force-output self)
       ;; (ensure-ssl-funcall stream (complement #'minusp) #'ssl-shutdown (sap self))
       (ssl-free (sap self))
       (setf (sap self) nil)
       (when (streamp (ssl-stream-socket self))
         (close (ssl-stream-socket self) :abort abort))
       (when-let ((f (ssl-stream-close-callback self)))
         (funcall f)))
       t)
    (t nil)))

(defmethod open-stream-p ((self ssl-stream))
  (sap self))

(defmethod stream-listen ((self ssl-stream))
  (or (peeked self)
      (setf (peeked self)
            (let* ((buf (input-buffer self))
                   (sap (sap self))
                   ;; (*bio-blockp* nil)
                   (n (with-vector-sap (ptr buf)
                        ;; (async-ssl-funcall self #'plusp #'ssl-read sap ptr 1)
                        )))
              (and (> n 0) (elt buf 0))))))

;; stream-read-byte
;; stream-read-sequence
;; stream-write-byte
;; stream-write-sequence
;; stream-finish-output
;; stream-force-output

;; (defun make-ssl-client-stream-from-fd (stream))
;; (defun make-ssl-client-stream (socket &rest args
;;                                       &key
;;                                       hostname
;;                                       close-callback
;;                                       external-format
;;                                       (verify (if (ssl-check-verify-p)
;;                                                   :optional
;;                                                   *make-ssl-client-stream-verify-default*))
;;                                       alpn-protocols
;;                                       certificate key password
;;                                       (cipher-list *default-cipher-list*)
;;                                       method
;;                                       (buffer-size *default-buffer-size*)
;;                                       (input-buffer-size buffer-size)
;;                                       (output-buffer-size buffer-size))
;;   (apply 'cl+ssl:make-ssl-client-stream socket args))
  

(defclass* ssl-server-stream (ssl-stream)
  (certificate key))

;; (defun make-ssl-server-stream-from-fd (stream))
;; (defun make-ssl-server-stream (socket &rest args
;;                                       &key
;;                                       close-callback
;;                                       external-format
;;                                       certificate key password
;;                                       (cipher-list *ssl-cipher-list*)
;;                                       method
;;                                       (buffer-size *default-buffer-size*)
;;                                       (input-buffer-size *ssl-buffer-size*)
;;                                       (output-buffer-size *ssl-buffer-size*))
;;   (apply 'cl+ssl:make-ssl-server-stream stream args))


;; (defmethod close ((stream ssl-stream) &key abort))
