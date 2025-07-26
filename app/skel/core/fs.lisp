;;; fs.lisp --- Skel Filesystem

;; Skel Filesystem: Logical Paths, URI Schemas, etc

;;; Code:
(in-package :skel/core/int)
(declaim (pathname *skel-stash* *skel-store*
               *skel-cache* *user-skelrc* *system-skelrc*))
(sb-ext:define-load-time-global *skel-stash* (merge-homedir-pathnames ".stash/skel/stash/"))
(sb-ext:define-load-time-global *skel-store* (merge-homedir-pathnames  ".stash/skel/store/"))
(sb-ext:define-load-time-global *skel-cache* (merge-homedir-pathnames ".stash/skel/cache/"))
(sb-ext:define-load-time-global *skel-data* (merge-homedir-pathnames ".stash/skel/data/"))
(sb-ext:define-load-time-global *skel-path* *default-pathname-defaults*)
(sb-ext:define-load-time-global *user-skelrc* (pathname (format nil "~~/~A" *default-skelrc*)))
(sb-ext:define-load-time-global *system-skelrc* (pathname "/etc/skelrc"))
