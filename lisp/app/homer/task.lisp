;;; task.lisp --- Homer Task Objects

;; Scheduled Tasks/Jobs

;;; Commentary:

;; The goal here is to provide a simple CRON-like API for running re-occuring
;; tasks concurrently

#|
:tasks
((mail-update (:repeat (:every (:min 15))) #$offlineimap -o$#)
 (shutdown () #$systemctl poweroff$#))
|#
;;; Code:
(in-package :homer/core)

(defclass homer-task (scheduled-task ast id) ())

(defmethod load-ast ((self homer-task))
  (with-slots (ast) self
    (setf (id self) (pop ast))
    (let ((props (pop ast)))
      (setf (schedule self) (getf props :repeat))
      ;; remainder of ast is evaulated when the task is executed
      (log:debug! "loaded task: ~A" (id self))
      self)))

(defmethod build-ast ((self homer-task) &key)
  (unless (equal (id self) (car (ast self)))
    (setf (ast self)
          `(,(id self) (:repeat ,(schedule self)) ,@(ast self)))))

(defmethod write-sxp-stream ((self homer-task) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(id self) (:repeat ,(schedule self)) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

;; (build-ast (load-ast (make-instance 'homer-task :ast '(mail-update (:repeat (:every (:min 15))) 1 2 3))))
  
(define-task-kernel homer-idle () ())

(define-task-kernel homer-compact () ())

(define-task-kernel homer-collect-tasks () ())

(defvar *homer-task-pool* (make-task-pool :kernel 'homer-idle :workers (make-workers (num-cpus))))
