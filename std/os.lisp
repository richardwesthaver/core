;;; std/os.lisp --- OS interop

;; OS-specific bits.

;;; Commentary:

;; Unix only.

;;; Code:
(in-package :std/os)
(require 'sb-posix)

(defvar *user* (sb-posix:getenv "USER"))

(defun sudo-p ()
  "Return T if effective user is root."
  (zerop (parse-integer (with-output-to-string (str) (sb-ext:process-output (sb-ext:run-program "id" (list "-u") :search t :output str)) 0))))

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

;; cat /sys/kernel/cpu_byteorder?

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

;;; Linux
;; https://man7.org/linux/man-pages/man3/statvfs.3.html
(define-alien-routine statvfs int
  (path c-string)
  (buf (* t)))

;; https://man7.org/linux/man-pages/man3/getmntent.3.html
(define-alien-type mntent 
  (struct mntent
          (fsname c-string)
          (dir c-string)
          (type c-string)
          (opts c-string)
          (freq int)
          (passno int)))

(define-alien-routine setmntent (* t) (filename c-string) (type c-string))

(define-alien-routine getmntent (* t) (stream (* t)))

(define-alien-routine endmntent int (stream (* t)))

(define-alien-routine hasmntopt c-string (mnt (* mntent)) (opt c-string))

;;; XDG

;; ref: https://freedesktop.org/wiki/Software/xdg-user-dirs/
(defvar *xdg-user-dirs* 
  (let ((tbl (make-hash-table)))
    (mapc (lambda (x) (setf (gethash (car x) tbl) (cdr x)))
          '((:desktop . "Desktop")
            (:download . "Downloads")
            (:templates . "Templates")
            (:publicshare . "Public")
            (:documents . "Documents")
            (:music . "Music")
            (:pictures . "Pictures")
            (:videos . "Videos")))
    tbl))

(defun xdg-user-dir (key) (gethash key *xdg-user-dirs*))

(defun (setf xdg-user-dir) (v k)
  (let ((new (if (typep v 'std/path:absolute-pathname)
                 v
                 (merge-pathnames v "~/"))))
    (setf (gethash k *xdg-user-dirs*) new)))

(defun init-xdg-user-dirs ()
  "Init *XDG-USER-DIRS* from environment."
  (mapc
   (lambda (k)
     (std/macs:when-let ((e (sb-posix:getenv (concatenate 'string "XDG_" (substitute #\_ #\- (string k)) "DIR"))))
       (setf (xdg-user-dir k) (pathname e))))
   (std/hash-table:hash-table-keys *xdg-user-dirs*))
  *xdg-user-dirs*)

;; ref: https://specifications.freedesktop.org/basedir-spec/latest/
(defvar *xdg-base-dirs*
  (let ((tbl (make-hash-table)))
    (mapc (lambda (x) (setf (gethash (car x) tbl) (cdr x)))
          `((:data-home . ".local/share")
            (:config-home ".config")
            (:state-home . ".local/state")
            (:data-dirs . (#p"/usr/local/share/" #p"/usr/share/"))
            (:config-dirs . (#P"/etc/xdg"))
            (:cache-home . (".cache"))
            (:runtime-dir)))
    tbl))

(defun xdg-base-dir (key) (gethash key *xdg-base-dirs*))

(defun (setf xdg-base-dir) (v k)
  (let ((new (if (typep v 'std/path:absolute-pathname)
                 v
                 (merge-pathnames v "~/"))))
    (setf (gethash k *xdg-base-dirs*) new)))

(defun init-xdg-base-dirs ()
  "Init *XDG-BASE-DIRS* from environment."
  (mapc
   (lambda (k)
     (std/macs:when-let ((e (sb-posix:getenv (concatenate 'string "XDG_" (substitute #\_ #\- (string k))))))
       (setf (xdg-base-dir k) (pathname e))))
   (std/hash-table:hash-table-keys *xdg-base-dirs*)))
