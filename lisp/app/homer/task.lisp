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

;; :sec :min :hour :day :day-of-week :month

(defun %parse-sched (sched)
  (unless (null sched)
    (let ((unit (pop sched))
          (val (pop sched))
          (now (now)))
      (timestamp+ now val unit))))
    
(defun parse-task-schedule (sched)
  "Parse a form as the schedule slot of a HOMER-TASK. The form may be a string
which is passed to PARSE-TIMESTRING or an integer which is processed as a unix
timestamp for a oneshot task, else it is a list."
  (etypecase sched
    (string (time:parse-timestring sched))
    (list (%parse-sched sched))))

(defmethod load-ast ((self homer-task))
  (with-slots (ast) self
    (setf (id self) (pop ast))
    (setf (schedule self) (pop ast))
    ;; remainder of ast is evaulated when the task is executed
    (log:debug! "loaded task: ~A" (id self))
    self))

(defmethod build-ast ((self homer-task) &key)
  (unless (equal (id self) (car (ast self)))
    (setf (ast self)
          `(,(id self) ,(schedule self) ,@(ast self)))))

(defmethod write-sxp-stream ((self homer-task) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(id self) (:repeat ,(schedule self)) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

;; (build-ast (load-ast (make-instance 'homer-task :ast '(mail-update (:repeat (:every (:min 15))) 1 2 3))))
  
(define-task-kernel homer-idle () ())

(define-task-kernel homer-compact () ())

(define-task-kernel homer-collect-tasks () ())

(defvar *homer-task-pool* (make-task-pool :kernel 'homer-idle :workers (make-workers (num-cpus))))

;;; Jobs
(defstruct (homer-job (:include sk-rule)))

(defmethod run-object ((self homer-job) &key)
  (when #1=(homer-job-source self)
    (mapc
     (lambda (j)
       (when-let ((job (find (string-upcase j) (jobs *home-config*)
                             :test 'equal
                             :key (lambda (x) (homer/core::homer-job-target x)))))
         (run-object job)))
     #1#))
  (mapcar 
   (lambda (x)
     (etypecase x
       ((or symbol function) (funcall x :output t))
       (t (eval x))))
   (homer-job-recipe self)))

(defmethod write-sxp-stream ((self homer-job) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(homer-job-target self) ,(homer-job-source self) ,@(homer-job-recipe self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))
