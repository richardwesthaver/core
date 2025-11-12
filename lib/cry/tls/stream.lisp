;;; stream.lisp --- SSL Streams

;; 

;;; Code:
(in-package :tls)

(defclass ssl-stream (wrapped-stream 
                      fundamental-binary-input-stream
                      fundamental-binary-output-stream)
  (socket sap))
(defmethod stream-element-type ((stream ssl-stream)) '(unsigned-byte 8))

(defclass ssl-client-stream (ssl-stream) ())
(defun make-ssl-client-stream-from-fd (stream))
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
  

(defclass ssl-server-stream (ssl-stream)
  (certificate key))

(defun make-ssl-server-stream-from-fd (stream))
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
