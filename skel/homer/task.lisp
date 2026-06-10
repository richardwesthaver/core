;;; task.lisp --- Homer Task Objects

;; Scheduled Tasks/Jobs

;;; Commentary:

;; The goal here is to provide a simple CRON-like API and runtime for
;; executing tasks.

#|
:tasks
((mail-update (:repeat (:every (:min 15))) #$offlineimap -o$#)
 (shutdown () #$systemctl poweroff$#))
|#
;;; Code:
(in-package :skel/homer/core)

(defkernel homer-task (scheduled-task ast id) ())

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
    (setf (task-schedule self) (pop ast))
    ;; remainder of ast is evaulated when the task is executed
    self))

(defmethod build ((self homer-task) &key)
  (unless (equal (id self) (car (ast self)))
    (setf (ast self)
          `(,(id self) ,(task-schedule self) ,@(ast self)))))

(defmethod write-ast ((self homer-task) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(id self) (:repeat ,(task-schedule self)) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

;; (build (load-ast (make-instance 'homer-task :ast '(mail-update (:repeat (:every (:min 15))) 1 2 3))))
  
;;; Jobs

(defkernel homer-job (simple-interactive-rule) ())

(defmethod exec ((self homer-job))
  (when #1=(source self)
    (mapc
     (lambda (j)
       (when-let ((job (find (string-upcase j) (jobs *home-config*)
                             :test 'equal
                             :key (lambda (x) (sink x)))))
         (exec job)))
     #1#))
  (mapcar 
   (lambda (x)
     (etypecase x
       ((or symbol function) (funcall x :output t))
       (t (eval x))))
   (ast self)))

(defmethod run-object ((self homer-job) &key)
  (exec self))

(defmethod write-ast ((self homer-job) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sink self) ,(source self) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))
