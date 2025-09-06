;;; cli/tests.lisp --- CLI Tests

;;

;;; Code:
(defpackage :cli/tests
  (:use :cl :std :rt :cli :cli/shell :cli/progress :cli/spark :cli/repl :cli/ansi :cli/clap :cli/tools/sbcl :obj/ast))

(in-package :cli/tests)
(defsuite :cli)
(in-suite :cli)

(deftest progress ()
  (flet ((%step () (update! 1)))
    (let ((*progress-bar-enabled* t)
          (n 100))
      (with-progress-bar (n "TEST: # of steps = ~a" n)
        (dotimes (i n) (%step))))))

(deftest spark ()
  (is (string= 
       (spark '(1 5 22 13 5))
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

(deftest repl (:skip t))

(deftest env ()
  (is (exec-path-list))
  (is (find-exe "sbcl")))

(deftest sbcl-tools ()
  (when (find-exe "sbcl")
    (iseql 
     :ok
     (read-from-string
      (with-output-to-string (s)
        (let ((cli/tools/sbcl::*sbcl-input* nil)
              (cli/tools/sbcl::*sbcl-output* s))
          (with-sbcl (:noinform t :quit t)
            (print :ok))))))))
