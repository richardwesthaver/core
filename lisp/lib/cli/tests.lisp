(defpackage :cli/tests
  (:use :cl :std :rt :cli :cli/progress :cli/spark :cli/repl :cli/ansi))

(in-package :cli/tests)
(defsuite :cli)
(in-suite :cli)


;; TODO: needs to be compiled outside scope of test - contender for
;; fixture API
(defprompt tpfoo "testing: ")
(defvar tcoll nil)
(defvar thist nil)

(deftest ansi () )

(deftest cli-prompt (:disabled t) ;; FIXME: hijacks io in slime
  "Test CLI prompts"
  (let ((*standard-input* (make-string-input-stream 
			   (format nil "~A~%~A~%~%" "foobar" "foobar"))))
    ;; prompts 
    (is (string= (tpfoo-prompt) "foobar"))
    (is (string= "foobar"
                 (cli:completing-read "nothing: " tcoll :history thist :default "foobar")))))

(defparameter *opts* (cli:make-opts (:name foo :global t :description "bar")
		       (:name bar :description "foo")))

(defparameter *cmd1* (make-cli :cmd :name "holla" :opts *opts* :description "cmd1 description"))
(defparameter *cmd2* (make-cli :cmd :name "ayo" :cmds #(*cmd1*) :opts *opts* :description "cmd1 description"))
(defparameter *cmds* (cli:make-cmds (:name "baz" :description "baz" :opts *opts*)))

(defparameter *cli* (make-cli t :opts *opts* :cmds *cmds* :description "test cli"))

(deftest cli ()
  "test MACS.CLI OOS."
  (let ((cli *cli*))
    (is (eq (make-shorty "test") #\t))
    (is (equalp (proc-args cli '("-f" "baz" "--bar" "fax")) ;; not eql
		(make-cli-ast 
		 (list (make-cli-node 'opt (find-short-opt cli #\f))
		       (make-cli-node 'cmd (find-cmd cli "baz"))
		       (make-cli-node 'opt (find-opt cli "bar"))
		       (make-cli-node 'arg "fax")))))
    (is (parse-args cli '("--bar" "baz" "-f" "yaks")))
    (is (stringp
	 (with-output-to-string (s)
	   (print-version cli s)
	   (print-usage cli s)
	   (print-help cli s))))
    (is (string= "foobar" (parse-str-opt "foobar")))))

(deftest progress ()
  (flet ((%step () (cli/progress::update 1)))
    (let ((*progress-bar-enabled* t)
          (n 100))
      (with-progress-bar (n "TEST: # of steps = ~a" n)
        (dotimes (i n) (%step))))))

(deftest spark ()
  (is (string= 
       (cli/spark:spark '(1 5 22 13 5))
       "▁▂█▅▂"))
  (is (string= 
       (spark '(5.5 20))
       "▁█"))
  (is (string=
       (spark '(1 2 3 4 100 5 10 20 50 300))
       "▁▁▁▁▃▁▁▁▂█"))
  (is (string=
       (spark '(1 50 100))
       "▁▄█"))
  (is (string=
       (spark '(2 4 8))
       "▁▃█"))
  (is (string=
       (spark '(1 2 3 4 5))
       "▁▂▄▆█"))
  (is (string=
       (spark '(0 30 55 80 33 150))
       "▁▂▃▄▂█"))
  ;; null
  (is (string=
       (spark '())
       ""))
  ;; singleton
  (is (string=
       (spark '(42))
       "▁"))
  ;; constant
  (is (string=
       (spark '(42 42))
       "▁▁"))
  ;; min/max
  (is (string=
       (spark '(0 30 55 80 33 150) :min -100)
       "▃▄▅▆▄█"))
  (is (string=
       (spark '(0 30 55 80 33 150) :max 50)
       "▁▅██▅█"))
  (is (string=
       (spark '(0 30 55 80 33 150) :min 30 :max 80)
       "▁▁▄█▁█"))
  ;; double-float, minus
  (is (string=
       (spark '(1.000000000005d0 0.000000000005d0 1.0d0))
       "█▁▇"))
  (is (string=
       (spark '(-1 0 -1))
       "▁█▁"))
  (is (string=
       (spark '(-1.000000000005d0 0.000000000005d0 -1.0d0))
       "▁█▁"))
  ;; *ticks*
  (let ((ternary '(-1 0 1 -1 1 0 0 -1 1 1 0)))
    (is (string=
         (spark ternary)
         "▁▄█▁█▄▄▁██▄"))
    (is (string=
         (let ((*ticks* #(#\_ #\- #\¯)))
           (spark ternary))
         "_-¯_¯--_¯¯-"))
    (is (string=
         (let ((*ticks* #(#\▄ #\⎯ #\▀)))
           (spark ternary))
         "▄⎯▀▄▀⎯⎯▄▀▀⎯"))
    (is (string=
         (let ((*ticks* #(#\E #\O)))
           (spark '(4 8 15 22 42) :key (lambda (n) (mod n 2))))
         "EEOEE")))
  ;; key
  (flet ((range (start end) (loop for i from start below end collect i))
         (fib (n) (loop for x = 0 then y
                        and y = 1 then (+ x y)
                        repeat n
                        finally (return x)))
         (fac (n) (labels ((rec (n acc) (if (<= n 1) acc (rec (1- n) (* n acc)))))
                    (rec n 1))))
    (is (string=
         (spark (range 0 51)
                :key (lambda (x) (sin (* x pi 1/4))))
         "▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█"))
    (is (string=
         (spark (range 0 51)
                :key (lambda (x) (cos (* x pi 1/4))))
         "█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄"))

    (is (string=
         (spark (range 0 51)
                :key (lambda (x) (abs (cis (* x pi 1/4)))))
         "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁"))
    
    (is (string=
         (spark (range 0 51)
                :key (lambda (x) (float (phase (cis (* x pi 1/4))) 1.0)))
         "▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆"))

    (is (string=
         (spark (range 1 7) :key #'log)   
         "▁▃▅▆▇█"))

    (is (string=
         (spark (range 1 7) :key #'sqrt)  
         "▁▃▄▅▆█"))
    (is (string=
         (spark (range 1 7))              
         "▁▂▃▅▆█"))
    (is (string=
         (spark (range 1 7) :key #'fib)   
         "▁▁▂▃▅█"))
    (is (string=
         (spark (range 1 7) :key #'exp)   
         "▁▁▁▁▃█"))
    (is (string=
         (spark (range 1 7) :key #'fac)   
         "▁▁▁▁▂█"))
    (is (string=
         (spark (range 1 7) :key #'isqrt) 
         "▁▁▁███"))
    ;; misc
    (flet ((lbits (n) (spark (map 'list #'digit-char-p (write-to-string n :base 2)))))
      (is (string=
           (lbits 42) 
           "█▁█▁█▁"))
      (is (string=
           (lbits 43) 
           "█▁█▁██"))
      (is (string=
           (lbits 44) 
           "█▁██▁▁"))
      (is (string= 
           (lbits 45) 
           "█▁██▁█")))

    ;; VSPARK
    (is (string=
         (vspark '())
         ""))
    ;; singleton
    (is (string= 
         (vspark '(1))
         "
1                      1.5                       2
˫-----------------------+------------------------˧
▏
"))

    ;; constant
    (is (string= 
         (vspark '(1 1))
         "
1                      1.5                       2
˫-----------------------+------------------------˧
▏
▏
"))


    (is (string=
         (vspark '(0 30 55 80 33 150))
         "
0                      75                      150
˫-----------------------+------------------------˧
▏
██████████▏
██████████████████▍
██████████████████████████▋
███████████▏
██████████████████████████████████████████████████
"))


    ;; min, max

    (is (string=
         (vspark '(0 30 55 80 33 150) :min -100)
         "
-100                    25                     150
˫-----------------------+------------------------˧
████████████████████▏
██████████████████████████▏
███████████████████████████████▏
████████████████████████████████████▏
██████████████████████████▋
██████████████████████████████████████████████████
"))

    (is (string=
         (vspark '(0 30 55 80 33 150) :max 50)
         "
0                      25                       50
˫-----------------------+------------------------˧
▏
██████████████████████████████▏
██████████████████████████████████████████████████
██████████████████████████████████████████████████
█████████████████████████████████▏
██████████████████████████████████████████████████
"))


    (is (string=
         (vspark '(0 30 55 80 33 150) :min 30 :max 80)
         "
30                      55                      80
˫-----------------------+------------------------˧
▏
▏
█████████████████████████▏
██████████████████████████████████████████████████
███▏
██████████████████████████████████████████████████
"))

    ;; labels
    (is (string=
         (vspark '(1 0 .5) :labels '("on" "off" "unknown")
                           :size 1
                           :scale? nil)
         "
     on █
    off ▏
unknown ▌
"))

    (is (string=
         (vspark '(1 0 .5) :labels '("on" "off")
                           :size 1
                           :scale? nil)
         "
 on █
off ▏
    ▌
"))

    (is (string=
         (vspark '(1 0) :labels '("on" "off" "unknown")
                        :size 1
                        :scale? nil)
         "
 on █
off ▏
"))

    ;; key
    (is (string=
         (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4))))
         "
-1.0                    0.0                    1.0
˫-----------------------+------------------------˧
█████████████████████████▏
██████████████████████████████████████████▋
██████████████████████████████████████████████████
██████████████████████████████████████████▋
█████████████████████████▏
███████▍
▏
███████▍
████████████████████████▉
"))

    ;; size
    (is (string=
         (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                                      :size 10)
         "
-1.0   1.0
˫--------˧
█████▏
████████▌
██████████
████████▌
█████▏
█▌
▏
█▌
████▉
"))

    ;; scale (mid-point)
    (is (string=
         (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                                      :size 20)
         "
-1.0     0.0     1.0
˫--------+---------˧
██████████▏
█████████████████▏
████████████████████
█████████████████▏
██████████▏
██▉
▏
██▉
█████████▉
"))

    (let ((life-expectancies '(("Africa" 56)
                               ("Americans" 76)
                               ("South-East Asia" 67)
                               ("Europe" 76)
                               ("Eastern Mediterranean" 68)
                               ("Western Pacific" 76)
                               ("Global" 70))))

      (is (string=
           (vspark life-expectancies :key #'second)
           "
56                      66                      76
˫-----------------------+------------------------˧
▏
██████████████████████████████████████████████████
███████████████████████████▌
██████████████████████████████████████████████████
██████████████████████████████▏
██████████████████████████████████████████████████
███████████████████████████████████▏
"))

      ;; newline?
      (is (string=
           (vspark life-expectancies :key #'second :scale? nil :newline? nil)
           "▏
██████████████████████████████████████████████████
███████████████████████████▌
██████████████████████████████████████████████████
██████████████████████████████▏
██████████████████████████████████████████████████
███████████████████████████████████▏"))

      ;; scale?
      (is (string=
           (vspark life-expectancies :key #'second :scale? nil)
           "
▏
██████████████████████████████████████████████████
███████████████████████████▌
██████████████████████████████████████████████████
██████████████████████████████▏
██████████████████████████████████████████████████
███████████████████████████████████▏
"))

      ;; labels
      (is (string=
           (vspark life-expectancies
                   :key   #'second
                   :labels (mapcar #'first life-expectancies))
           "
                      56           66           76
                      ˫------------+-------------˧
               Africa ▏
            Americans ████████████████████████████
      South-East Asia ███████████████▍
               Europe ████████████████████████████
Eastern Mediterranean ████████████████▊
      Western Pacific ████████████████████████████
               Global ███████████████████▋
"))

      ;; title
      (is (string=
           (vspark life-expectancies
                   :min 50 :max 80
                   :key    #'second
                   :labels (mapcar #'first life-expectancies)
                   :title "Life Expectancy")
           "
                 Life Expectancy                  
                      50           65           80
                      ˫------------+-------------˧
               Africa █████▋
            Americans ████████████████████████▎
      South-East Asia ███████████████▉
               Europe ████████████████████████▎
Eastern Mediterranean ████████████████▊
      Western Pacific ████████████████████████▎
               Global ██████████████████▋
"))

      (is (string=
           (spark (range 0 15) :key #'fib)
           "▁▁▁▁▁▁▁▁▁▁▂▂▃▅█"))

      (is (string=
           (vspark (range 0 15) :key #'fib)
           "
0                    188.5                     377
˫-----------------------+------------------------˧
▏
▏
▏
▎
▍
▋
█▏
█▊
██▊
████▌
███████▍
███████████▊
███████████████████▏
██████████████████████████████▉
██████████████████████████████████████████████████
")))))

(deftest repl ())
