;;; std/os.lisp --- OS interop

;; OS-specific bits.

;;; Commentary:

;; Unix only.

;;; Code:
(in-package :std/os)
(require 'sb-posix)

(defparameter *user* (sb-posix:getenv "USER"))

(defun sudo-p ()
  "Return T if effective user is root."
  (zerop (parse-integer (with-output-to-string (str) (sb-ext:process-output (sb-ext:run-program "id" (list "-u") :search t :output str)) 0))))

(defun user-info (id)
  "USER-INFO returns the password entry for the given name or
numerical user ID, as an assoc-list."
  (multiple-value-bind (name password uid gid gecos home shell)
      (etypecase id
        (string (sb-posix:getpwnam id))
        (integer (sb-posix:getpwuid id)))
    (declare (ignore password))
    (unless (null name)
      (list (cons :name name)
            (cons :user-id uid)
            (cons :group-id gid)
            (cons :gecos gecos)
            (cons :home home)
            (cons :shell shell)))))

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

(define-alien-routine isatty int (fd int))

(define-alien-routine tcsetattr int (fd int) (actions int) (term (* t)))
(define-alien-routine cfmakeraw void (term (* t)))
(define-alien-type winsize (struct winsize
			     (row unsigned-short)
			     (col unsigned-short)
			     (xpixel unsigned-short)
			     (ypixel unsigned-short)))

;; #define TIOCGWINSZ	0x5413
;; #define TIOCSWINSZ	0x5414
;; #define TIOCNOTTY	0x5422
(defconstant +tiocgwinsz+ #x5413)
(defconstant +tiocswinsz+ #x5414)
(defconstant +tiocnotty+ #x5422)
(defconstant +tcsanow+ 0)
(defconstant +tcsadrain+ 1)
(defconstant +tcsaflush+ 2)
(defconstant +opost+ #x01)

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

;;; with-directory-iterator
(defun %get-file-kind (namestring follow-p)
  (handler-case
      (let ((mode (sb-posix:stat-mode
                   (if follow-p
                       (sb-posix:stat namestring)
                       (sb-posix:lstat namestring)))))
        (case (logand sb-posix:s-ifmt mode)
          (#.sb-posix:s-ifdir  :directory)
          (#.sb-posix:s-ifchr  :character-device)
          (#.sb-posix:s-ifblk  :block-device)
          (#.sb-posix:s-ifreg  :regular-file)
          (#.sb-posix:s-iflnk  :symbolic-link)
          (#.sb-posix:s-ifsock :socket)
          (#.sb-posix:s-ififo  :pipe)
          (otherwise
           (error "Unknown file mode: ~A." mode))))
    ;; TODO 2025-03-24: test
    (sb-posix:syscall-error ()
      (cond
        ;; stat() returned ENOENT: either FILE does not exist
        ;; or the end of the symlink chain is a broken symlink
        (follow-p
         (handler-case
             (sb-posix:lstat namestring)
           (:no-error (stat)
             (declare (ignorable stat))
             (values :symbolic-link :broken))))
        ;; lstat() returned ENOENT: FILE does not exist
        (t nil)))))

(defun get-file-kind (file follow-p)
  (%get-file-kind (sb-ext:native-namestring file) follow-p))

;;;; Hopefully portable pathname manipulations

(defun absolute-pathname-p (pathspec)
  "Returns T if the PATHSPEC designates an absolute pathname, NIL otherwise."
  (eq :absolute (car (pathname-directory pathspec))))

(defun relative-pathname-p (pathspec)
  "Returns T if the PATHSPEC designates a relative pathname, NIL otherwise."
  (not (absolute-pathname-p pathspec)))

(defun absolute-pathname (pathspec
                          &optional (default *default-pathname-defaults*))
  "Returns an absolute pathname corresponding to PATHSPEC by
merging it with DEFAULT, and (CURRENT-DIRECTORY) if necessary."
  (if (relative-pathname-p pathspec)
      (let ((tmp (merge-pathnames
                  pathspec
                  (make-pathname :name nil :type nil :version nil
                                 :defaults default))))
        (if (relative-pathname-p tmp)
            (merge-pathnames tmp (current-directory))
            tmp))
      pathspec))

(defun unmerge-pathnames (pathspec
                          &optional (default *default-pathname-defaults*))
  "Removes those leading directory components from PATHSPEC that
are shared with DEFAULT."
  (let* ((dir (pathname-directory pathspec))
         (mismatch (mismatch dir (pathname-directory default) :test #'equal)))
    (make-pathname :directory (when mismatch
                                `(:relative ,@(subseq dir mismatch)))
                   :defaults pathspec)))

(defun current-directory ()
  "CURRENT-DIRECTORY returns the operating system's current
directory, which may or may not correspond to
*DEFAULT-PATHNAME-DEFAULTS*.

SETF CURRENT-DIRECTORY changes the operating system's current
directory to the PATHSPEC.  An error is signalled if the PATHSPEC
is wild or does not designate a directory."
  (let ((cwd (sb-posix:getcwd)))
    (if cwd
        (pathname (concatenate 'string cwd "/"))
        (error "Could not get current directory."))))

(defun (setf current-directory) (pathspec)
  (sb-posix:chdir pathspec))

(defun call-with-directory-iterator (pathspec fun)
  (let ((dir (absolute-pathname (pathname pathspec)))
        (old-dir (current-directory)))
    (let ((dp (sb-posix:opendir dir)))
      (labels ((one-iter ()
                 (let ((name (sb-posix:dirent-name (sb-posix:readdir dp))))
                   (unless (null name)
                     (cond
                       ((member name '("." "..") :test #'string=)
                        (one-iter))
                       ((eq :directory (%get-file-kind name t))
                        (make-pathname :directory `(:relative ,name)))
                       (t
                        (let ((dotpos (position #\. name :from-end t)))
                          (if (and dotpos (plusp dotpos))
                              (make-pathname :name (subseq name 0 dotpos)
                                             :type (subseq name (1+ dotpos)))
                              (make-pathname :name name)))))))))
        (unwind-protect
             (let ((*default-pathname-defaults* dir))
               (setf (current-directory) dir)
               (funcall fun #'one-iter))
          (sb-posix:closedir dp)
          (setf (current-directory) old-dir))))))

(defmacro with-directory-iterator ((iterator pathspec) &body body)
  "PATHSPEC must be a valid directory designator:
*DEFAULT-PATHNAME-DEFAULTS* is bound, and (CURRENT-DIRECTORY) is set
to the designated directory for the dynamic scope of the body.

Within the lexical scope of the body, ITERATOR is defined via
macrolet such that successive invocations of (ITERATOR) return
the directory entries, one by one.  Both files and directories
are returned, except '.' and '..'.  The order of entries is not
guaranteed.  The entries are returned as relative pathnames
against the designated directory.  Entries that are symbolic
links are not resolved, but links that point to directories are
interpreted as directory designators.  Once all entries have been
returned, further invocations of (ITERATOR) will all return NIL.

The value returned is the value of the last form evaluated in
body.  Signals an error if PATHSPEC is wild or does not designate
a directory."
  (with-gensyms (one-iter)
    `(call-with-directory-iterator
      ,pathspec
      (lambda (,one-iter)
        (declare (type function ,one-iter))
        (macrolet ((,iterator ()
                     `(funcall ,',one-iter)))
          ,@body)))))

(defun file-kind (pathspec &key follow-symlinks)
  "Returns a keyword indicating the kind of file designated by PATHSPEC,
or NIL if the file does not exist.  Does not follow symbolic
links by default.

Possible file-kinds in addition to NIL are: :REGULAR-FILE,
:SYMBOLIC-LINK, :DIRECTORY, :PIPE, :SOCKET, :CHARACTER-DEVICE, and
:BLOCK-DEVICE.
If FOLLOW-SYMLINKS is non-NIL and PATHSPEC designates a broken symlink
returns :BROKEN as second value.

Signals an error if PATHSPEC is wild."
  (get-file-kind (merge-pathnames pathspec) follow-symlinks))
