;;; uring.asd-*- mode: lisp; -*-
(in-package :sys.uring)
(defsystem :uring
  :depends-on (sb-grovel)
  :components ((grovel-constants-file "uring/cs" :package :uring)
	       (grovel-constants-file "uring/cs.unix" :package :uring)
	       (:file "uring/uring")))
