#!/usr/bin/env -S core --script
(in-readtable :shell)
#$install -C -m 755 .stash/core /usr/bin/core$#
(let ((apps '("skel" "homer" "mpk" "pod")))
  (with-progress-bar ((length apps) "installing core to /usr/bin/core")
    (loop for i in apps
          while i
          do (run-program "/bin/ln" `("-sf" "/usr/bin/core" ,(format nil "/usr/bin/~A" i)))
          do (update! 1))))
