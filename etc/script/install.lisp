#!/usr/bin/env -S core --script
(in-readtable :shell)

(let ((apps '("skel" "homer" "mpk" "krypt" "packy")))
  (with-progress-bar ((+ (length apps) 2) "installing core to /usr/bin/core")
    #$install -C -m 644 -D etc/lisp/* /etc/lisp$#
    (update! 1)
    #$install -C -m 755 .stash/core /usr/bin/core$#
    (update! 1)
    (loop for i in apps
          while i
          do (run-program "/bin/ln" `("-sf" "/usr/bin/core" ,(format nil "/usr/bin/~A" i)))
          do (update! 1))))
