(in-package :pod)

(defclass live-exec (id)
  (stderr stdin stdout cmd detachkeys env privileged tty user working-dir))
