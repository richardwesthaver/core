;;; cli/tests.lisp --- CLI Tests

;;

;;; Code:
(defpackage :cli/tests
  (:use :cl :std :rt :cli :cli/shell :cli/progress :cli/spark :cli/repl :cli/ansi :cli/prompt :cli/clap :cli/tools/sbcl))

(in-package :cli/tests)
(declaim (optimize (debug 3) (safety 3)))
(defsuite :cli)
(in-suite :cli)

(defun ansi-t01 ()
  (erase)
    (cursor-position 0 0)
    (princ "0")
    (cursor-position 2 2)
    (princ "1")
    (cursor-position 5 15)
    (princ "test")
    (cursor-position 10 15)
    (force-output)
  (with-input-from-string (in (format nil "test~%~%"))
    (let ((a (read-line in)))
      (cursor-position 12 15)
      (princ a)
      (force-output))))

(defun ansi-t02 ()
  (print "normal")
  (.sgr 1)
  (print "bold")
  (.sgr 4)
  (print "bold underline")
  (.sgr 7)
  (print "bold underline reverse")
  (.sgr 22)
  (print "underline reverse")
  (.sgr 24)
  (print "reverse")
  (.sgr 27)
  (print "normal")
  (.sgr 1 4 7)
  (print "bold underline reverse")
  (.sgr 0)
  (print "normal")
  (force-output))

(defun ansi-t03 ()
  "Display the 256 color palette."
  (clear)
  (loop for i from 0 to 255 do
           (.sgr 48 5 i)
           (princ #\space))
  (terpri)
  (.sgr 0)
  (loop for i from 0 to 255 do
           (.sgr 38 5 i)
           (princ "X"))
  (.sgr 0)
  (force-output)
  (sleep 3)
  (.ris)
  (force-output))

(defun ansi-t04 ()
  "Hide and show the cursor."
  (princ "Cursor visible:")
  (force-output)
  (sleep 2)
  (terpri)
  (princ "Cursor invisible:")
  (hide-cursor)
  (force-output)
  (sleep 2)
  (terpri)
  (princ "Cursor visible:")
  (show-cursor)
  (force-output)
  (sleep 2))

(defun ansi-t05 ()
  "Switch to and back from the alternate screen buffer."
  (princ "Normal screen buffer. ")
  (force-output)
  (sleep 2)
  (save-cursor-position)
  (use-alternate-screen-buffer)
  (clear)
  (princ "Alternate screen buffer.")
  (force-output)
  (sleep 2)
  (use-normal-screen-buffer)
  (restore-cursor-position)
  (princ "Back to Normal screen buffer.")
  (force-output)
  (sleep 1))

(defun ansi-t06 ()
  "Set individual termios flags to enable raw and disable echo mode.

Enabling raw mode allows read-char to return immediately after a key is pressed.

In the default cooked mode, the entry has to be confirmed by pressing enter."
  (set-tty-mode t :ignbrk nil
                  :brkint nil
                  :parmrk nil
                  :istrip nil
                  :inlcr  nil
                  :igncr  nil
                  :icrnl  nil
                  :ixon   nil
                  :opost  nil
                  :echo   nil
                  :echonl nil
                  :icanon nil
                  :isig   nil
                  :iexten nil
                  :csize  nil
                  :parenb nil
                  :vmin 1
                  :vtime 0)
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 10 5)
    (princ a)
    (force-output))

  (set-tty-mode t :echo t
                  :brkint t
                  :ignpar t
                  :istrip t
                  :icrnl t
                  :ixon t
                  :opost t
                  :isig t
                  :icanon t
                  :veol 0))

(defun ansi-t07 ()
  "Use combination modes that consist of several individual flags.

Cooked and raw are opposite modes. Enabling cooked disbles raw and vice versa."
  (set-tty-mode t :cooked nil)
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 3 1)
    (princ a)
    (force-output))
  (set-tty-mode t :raw nil))

(defun ansi-t08 ()
  "Why doesnt calling the stty utility work?"
  (uiop:run-program "stty raw -echo" :ignore-error-status t)
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 2 1)
    (princ a)
    (force-output))
  (uiop:run-program "stty -raw echo" :ignore-error-status t))

(defun ansi-t09 ()
  "Query terminal size with ANSI escape sequences."
  ;; Put the terminal into raw mode so we can read the "user input"
  ;; of the reply char by char
  ;; Turn off the echo or the sequence will be displayed
  (set-tty-mode t :cooked nil :echo nil)
  (save-cursor-position)
  ;; Go to the bottom right corner of the terminal by attempting
  ;; to go to some high value of row and column
  (cursor-position 999 999)
  (let (chars)
    ;; The terminal returns an escape sequence to the standard input
    (device-status-report)
    (force-output)
    ;; The reply isnt immediately available, the terminal does need
    ;; some time to answer
    (sleep 0.1)
    ;; The reply has to be read as if the user typed an escape sequence
    (loop for i = (read-char-no-hang *standard-input* nil)
          until (null i)
          do (push i chars))
    ;; Put the terminal back into its initial cooked state
    (set-tty-mode t :raw nil :echo t)
    (restore-cursor-position)
    ;; Return the read sequence as a list of characters.
    (nreverse chars)))

(deftest ansi ()
  (with-input-from-string (in (format nil "~%~%"))
    (ansi-t01)
    (ansi-t02)
    (ansi-t03)
    (ansi-t04)
    (ansi-t05)))

;; TODO: needs to be compiled outside scope of test - contender for
;; fixture API
(defprompt tpfoo :prompt "testing:")

(deftest cli-prompt ()
  "Test CLI prompts"
  (defvar tcoll nil)
  (defvar thist nil)
  (let ((*standard-input* (make-string-input-stream 
			   (format nil "~A~%~A~%~%" "foobar" "foobar"))))
    ;; prompts 
    (is (string= (tpfoo-prompt) "foobar"))
    (is (string= "foobar"
                 (completing-read "nothing: " tcoll :history thist :default "foobar")))))

(defparameter *opts* '((:name "foo" :global t :description "bar")
		       (:name "bar" :description "foo")))

(defparameter *cmd1* (make-cli :cmd :name "holla" :opts *opts* :description "cmd1 description"))
(defparameter *cmd2* (make-cli :cmd :name "ayo" :cmds #(*cmd1*) :opts *opts* :description "cmd1 description"))
(defparameter *cmds* (make-cmds '(:name "baz" :description "baz" :opts *opts*)))

(defparameter *cli* (make-cli :cli :opts *opts* :cmds *cmds* :description "test cli"))

(deftest clap-basic ()
  "test basic CLAP functionality."
  (let ((cli *cli*))
    (is (eq (make-shorty "test") #\t))
    (is (equalp (proc-args cli '("-f" "baz" "--bar" "fax")) ;; not eql
		(make-cli-ast 
		 (list (make-cli-node 'opt (find-short-opts cli #\f))
		       (make-cli-node 'cmd (find-cmd cli "baz"))
		       (make-cli-node 'opt (find-opts cli "bar"))
		       (make-cli-node 'arg "fax")))))
    (is (parse-args cli '("--bar" "baz" "-f" "yaks")))
    (is (stringp
	 (with-output-to-string (s)
	   (print-version cli s)
	   (print-usage cli s)
	   (print-help cli s))))
    (is (string= "foobar" (cli/clap::parse-string-opt "foobar")))))

(make-opt-parser thing *arg*)

(deftest clap-opts ()
  "CLAP opt tests."
  (is (reduce (lambda (x y) (when x (when y t)))
              (loop for k across *cli-opt-kinds* collect (cli-opt-kind-p k))))
  (is (parse-thing-opt t))
  (is (null (parse-thing-opt nil))))

(deftest progress ()
  (flet ((%step () (cli/progress::update 1)))
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

(deftest repl ())

(deftest env ()
  (is (ld-library-path-list))
  (is (exec-path-list))
  (is (find-exe "sbcl")))

(deftest clap-ast ())

(compile (defmain (:exit nil :export nil)
           (let ((test-target t))
             test-target)))

(deftest main-output ()
  (is (not (funcall 'main))))

(deftest sbcl-tools ()
  (with-sbcl (:noinform t :quit t)
    (print 1)))
