;;; std/os.lisp --- OS interop definitions

;; mostly POSIX stuff. Windows is not supported.

;;; Code:
(in-package :std)
(require 'sb-posix)

(defun list-all-users ()
  "List all users via passwd. (uid gid name home shell comment)"
  (let ((r nil))
    (sb-posix:do-passwds (u r) 
      (push (list (sb-posix:passwd-uid u)
                  (sb-posix:passwd-gid u)
                  (sb-posix:passwd-name u)
                  (sb-posix:passwd-dir u)
                  (sb-posix:passwd-shell u)
                  (sb-posix:passwd-gecos u))
            r))
    r))

(defun list-all-groups ()
  "List all groups. (gid name mem)"
  (let ((r nil))
  (sb-posix:do-groups (g r) (push (list (sb-posix:group-gid g)
                                        (sb-posix:group-name g)
                                        (sb-posix:group-mem g))
                                  r))))
