;;; threads.lisp --- Multi-thread utilities

;; Threading Macros

;;; Commentary:

;; mostly yoinked from sb-thread and friends

;;; Code:
(in-package :std)

(defvar *thread-id-map-lock* (make-mutex :name "thread-id-map"))

(defvar *thread-id-map* (make-hash-table))

(defun thread-support-p () (member :thread-support *features*))

(defun print-thread-info (&optional (stream *standard-output*))
  (let* ((curr-thread sb-thread:*current-thread*)
         (curr-thread-name (sb-thread:thread-name curr-thread))
         (all-threads (sb-thread:list-all-threads)))
	(format stream "Current thread: ~a~%~%" curr-thread)
	(format stream "Current thread name: ~a~%~%" curr-thread-name)
	(format stream "All threads:~% ~{~a~%~}~%" all-threads)))

(eval-when (:compile-toplevel)
  (defun print-thread-message-top-level (msg)
    (sb-thread:make-thread
     (lambda ()
       (format #.*standard-output* msg)))
    nil))

;; this is all very unsafe. don't touch the finalizer thread plz.
(defun find-thread (id)
  "Search for thread by ID which must be an u64. On success returns the thread itself or nil."
  (sb-thread::avlnode-data (sb-thread::avl-find id sb-thread::*all-threads*)))

(defun thread-id-list ()
  (sb-thread::avltree-filter #'sb-thread::avlnode-key sb-thread::*all-threads*))

(defun thread-count ()
  (sb-thread::avl-count sb-thread::*all-threads*))

(defmacro def-thread (name)
  `(progn
     (defstruct (,name
                 (:copier nil)
                 (:include thread (%name ,(string-downcase (symbol-name name))))
                 (:constructor ,(symbolicate 'make- name))
                 (:conc-name "THREAD-")))))

(defun make-threads (n name fn)
  (loop for i from 1 to n
        collect (make-thread fn :name (format nil "~A-~D" name i))))

(defun timed-join-thread (thread &optional (timeout +timeout+))
  (handler-case (sb-sys:with-deadline (:seconds timeout)
                  (join-thread thread :default :aborted))
    (sb-ext:timeout ()
      :timeout)))

(defun hang ()
  (join-thread *current-thread*))

(defun kill-thread (thread)
  (when (thread-alive-p thread)
    (ignore-errors
      (terminate-thread thread))))
