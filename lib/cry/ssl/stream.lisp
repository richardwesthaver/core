;;; stream.lisp --- SSL Streams

;; 

;;; Code:
(in-package :ssl)

(defclass ssl-stream (wrapped-stream 
                      fundamental-binary-input-stream
                      fundamental-binary-output-stream)
  ())

;; ssl-server-stream
