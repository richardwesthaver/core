;;; c2.el --- remote execution of elisp -*- lexical-binding: t -*-
;; Copyright (C) 2021-2024  ellis
;; 
;; Author: ellis
;; Keywords: local, vc, net, process
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; 
;; Commentary:
;; 
;; This package provides functions for executing elisp on a running
;; emacs instance remotely.
;; 
;;; Code:
(defgroup c2 nil
  "elisp server")

(defcustom c2-directory (join-paths user-stash-directory "c2") "c2 directory."
  :group 'c2)

(defcustom c2-after-make-frame-hook nil
  "Hook run when c2 creates a client frame.
The created frame is selected when the hook is called."
  :type 'hook
  :group 'c2)

(defcustom c2-done-hook nil
  "Hook run when done editing a buffer with c2."
  :type 'hook
  :group 'c2)

(defcustom c2-port 82824
  "port of the c2 broadcaster"
  :group 'c2)

(defvar c2-process nil
  "The c2 process handle.")

(defvar c2-clients nil
  "List of current c2 clients.
Each element is a process.")

;;; Bindat
(setq c2-header-bindat-spec
      '((dest-ip   ip)
        (dest-port u16)
        (src-ip    ip)
        (src-port  u16)))

(setq c2-body-bindat-spec
      '((type      u8)
        (opcode    u8)
        (length    u16)
        (id        strz 8)
        (data      vec (length))
        (align     4)))

(setq c2-packet-bindat-spec
      '((header    struct header-spec)
        (counters  vec 2 u32r)
        (items     u8)
        (fill      3)
        (item      repeat (items)
                   (struct data-spec))))

(defun c2-insert-string (string)
  (insert string 0 (make-string (- 3 (% (length string) 4)) 0)))

(defun c2-insert-int32 (value)
  (let (bytes)
    (dotimes (i 4)
      (push (% value 256) bytes)
      (setq value (/ value 256)))
    (dolist (byte bytes)
      (insert byte))))

(defun c2-insert-float32 (value)
  (let (s (e 0) f)
    (cond
     ((string= (format "%f" value) (format "%f" -0.0))
      (setq s 1 f 0))
     ((string= (format "%f" value) (format "%f" 0.0))
      (setq s 0 f 0))
     ((= value 1.0e+INF)
      (setq s 0 e 255 f (1- (expt 2 23))))
     ((= value -1.0e+INF)
      (setq s 1 e 255 f (1- (expt 2 23))))
     ((string= (format "%f" value) (format "%f" 0.0e+NaN))
      (setq s 0 e 255 f 1))
     (t
      (setq s (if (>= value 0.0)
		  (progn (setq f value) 0)
		(setq f (* -1 value)) 1))
      (while (>= (* f (expt 2.0 e)) 2.0) (setq e (1- e)))
      (if (= e 0) (while (< (* f (expt 2.0 e)) 1.0) (setq e (1+ e))))
      (setq f (round (* (1- (* f (expt 2.0 e))) (expt 2 23)))
	    e (+ (* -1 e) 127))))
    (insert (+ (lsh s 7) (lsh (logand e #XFE) -1))
	    (+ (lsh (logand e #X01) 7) (lsh (logand f #X7F0000) -16))
	    (lsh (logand f #XFF00) -8)
	    (logand f #XFF))))

(defun c2-read-string ()
  (let ((pos (point)) string)
    (while (not (= (following-char) 0)) (forward-char 1))
    (setq string (buffer-substring-no-properties pos (point)))
    (forward-char (- 4 (% (length string) 4)))
    string))

(defun c2-read-int32 ()
  (let ((value 0))
    (dotimes (i 4)
      (setq value (logior (* value 256) (following-char)))
      (forward-char 1))
    value))

(defun c2-read-float32 ()
  (let ((s (lsh (logand (following-char) #X80) -7))
	(e (+ (lsh (logand (following-char) #X7F) 1)
	      (lsh (logand (progn (forward-char) (following-char)) #X80) -7)))
	(f (+ (lsh (logand (following-char) #X7F) 16)
	      (lsh (progn (forward-char) (following-char)) 8)
	      (prog1 (progn (forward-char) (following-char)) (forward-char)))))
    (cond
     ((and (= e 0) (= f 0))
      (* 0.0 (expt -1 s)))
     ((and (= e 255) (or (= f (1- (expt 2 23))) (= f 0)))
       (* 1.0e+INF (expt -1 s)))
     ((and (= e 255) (not (or (= f 0) (= f (1- (expt 2 23))))))
      0.0e+NaN)
     (t
      (* (expt -1 s)
	 (expt 2.0 (- e 127))
	 (1+ (/ f (expt 2.0 23))))))))

;;; Network
;;;###autoload
(defun net-check-opts ()
  ;; https://gnu.huihoo.org/emacs/24.4/emacs-lisp/Network-Options.html#Network-Options
  ;; non-blocking
  (featurep 'make-network-process '(:nowait t))
  ;; UNIX socket
					;(featurep 'make-network-process '(:family local))
  ;; UDP
  (featurep 'make-network-process '(:type datagram)))

;;; Process
(defun c2-make-client (host port)
  (make-network-process
   :name "c2-client"
   :coding 'binary
   :host host
   :service port
   :type 'datagram
   :nowait t))

(defun c2-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (setq c2-clients (assq-delete-all proc c2-clients))
    (c2-log (format "client %s has quit" proc))))

;;from server.el
(defun c2-log (string &optional client)
  "If a *c2* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*c2*")
      (with-current-buffer "*c2*"
        (goto-char (point-max))
        (insert (if client (format "<%s>: " (format-network-address (process-datagram-address client))))
                string)
        (or (bolp) (newline)))))

;;;###autoload
(defun c2-start ()
  "start c2 over udp"
  (interactive)
  (unless (process-status "c2")
    (make-network-process :name "c2"
			  :buffer "*c2*"
			  :family 'ipv4
			  :service c2-port
			  :type 'datagram
			  :coding 'binary
			  :sentinel 'c2-sentinel
			  :filter 'c2-filter
			  :server t
			  :broadcast t) 
    (setq c2-clients '())
    ;; setup additional filters
    (add-function :after (process-filter (get-process "c2")) #'c2-eval-response-filter))
  (message "c2: ONLINE"))

;;;###autoload
(defun c2-stop ()
  "stop the c2 server."
  (interactive)
  (while  c2-clients
    (delete-process (car (car c2-clients)))
    (setq c2-clients (cdr c2-clients)))
  (with-current-buffer "*c2*"
    (let ((proc (get-buffer-process (current-buffer))))
      (if proc (delete-process proc)))
    (set-buffer-modified-p nil)
    (kill-this-buffer))
  (message "c2 stopped"))

(defun c2-filter (proc string)   
  (let ((pending (assoc proc c2-clients))
        message
        index)
    ;;create entry if required
    (unless pending
      (setq c2-clients (cons (cons proc "") c2-clients))
      (setq pending  (assoc proc c2-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
;      (process-send-string proc (substring message 0 index))
      (c2-log  (substring message 0 index) proc)
      (setq message (substring message index)))
    (setcdr pending message)))

(defun c2-packet-filter (proc string)
  "process-filter for decoding 'c2-packet-bindat-spec'"
  (bindat-unpack packet-spec string))

(defun ordinary-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))

        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun c2-eval-response-filter (proc string)
  "execute STRING from PROC."
  (let ((msg (car (read-from-string string))))
    (process-send-string proc (concat (format "%s" (ignore-errors "error: %S" (eval msg))) "\n"))))

;;;; Signals
;;;###autoload
(defun c2-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;;###autoload
(defun c2-restart ()
  "Handler for SIGUSR1 signal, to (re)start an emacs server.

Can be tested from within emacs with:
  (signal-process (emacs-pid) 'sigusr1)

or from the command line with:
$ kill -USR1 <emacs-pid>
$ emacsclient -c
"
  (interactive)
  (server-force-delete)
  (server-start)
  )

(define-key special-event-map [sigusr1] 'c2-restart)

(provide 'c2)
;;; c2.el ends here
