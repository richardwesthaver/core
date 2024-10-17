;;; std/os.lisp --- OS interop

;; OS-specific bits.

;;; Commentary:

;; Unix only.

;;; Code:
(in-package :std/os)
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

(defmacro with-umask (mask &body body)
  "Temporarily set the system-wide umask for the extent of BODY."
  (with-gensyms (umask)
    `(let ((,umask (sb-posix:umask ,mask)))
       (unwind-protect (progn ,@body)
         (sb-posix:umask ,umask)))))

;; (with-umask #o22 nil)

(defmacro with-fd ((fvar fname &key (flags #.sb-posix:o-rdonly) (close t)) &body body)
  "Bind FVAR to an open file descriptor resulting from calling SB-POSIX:OPEN on
FNAME with FLAGS for the duration of BODY. When CLOSE is non-nil (the default)
arrange for FVAR to be closed after BODY."
  `(let* ((,fvar (sb-posix:open ,fname ,flags)))
     (unwind-protect (progn ,@body)
       ,@(when close `(sb-posix:close ,fvar)))))
