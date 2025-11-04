;;; std/os.lisp --- OS interop -*- allout-layout: (0) -*-

;; OS-specific bits.

;;; Commentary:

;; Unix only.

;;; Code:
(in-package :std/os)
(require 'sb-posix)

(defun current-user () 
  "The name of the currently logged-in user."
  (sb-posix::getenv "USER"))

(definline get-host-name () (sb-unix:unix-gethostname))
  
(defun sudo-p ()
  "Return T if effective user is root."
  (zerop (parse-integer (with-output-to-string (str) (sb-ext:process-output (sb-ext:run-program "id" (list "-u") :search t :output str)) 0))))

(defun forkable-p ()
  "Return T if it is possible to fork the current process (must have only one thread running)."
  (null (cdr (sb-thread:list-all-threads))))

(defun user-info (&optional (id (sb-posix:getuid)))
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

;;;_ Linux
;; https://man7.org/linux/man-pages/man3/statvfs.3.html
(defar statvfs int
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

(defar setmntent (* t) (filename c-string) (type c-string))

(defar getmntent (* t) (stream (* t)))

(defar endmntent int (stream (* t)))

(defar hasmntopt c-string (mnt (* mntent)) (opt c-string))

;; also defined in sb-unix
(defar isatty int (fd int))

(defar ("tcgetattr" tcgetattr*) int (fd int) (term (* sb-posix::alien-termios)))
(defar ("tcsetattr" tcsetattr*) int (fd int) (actions int) (term (* sb-posix::alien-termios)))
(defar cfmakeraw void (term (* sb-posix::alien-termios)))

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

;;;_ IOCTLs
;; based on functions from Shinmera's CL-SPIDEV
;; TODO 2025-04-27: 
(defun ioctl (fd cmd)
  (sb-alien:with-alien ((result sb-alien:int))
    (multiple-value-bind (wonp error)
        (sb-unix:unix-ioctl fd
                            (if (< cmd (expt 2 31)) cmd (- cmd (expt 2 32)))
                            (sb-alien:alien-sap (sb-alien:addr result)))
      (unless wonp
        (error "IOCTL ~a failed: ~a" cmd (sb-impl::strerror error))))
    result))

(defun (setf ioctl) (arg fd cmd)
  (sb-alien:with-alien ((value sb-alien:int))
    (setf value arg)
    (multiple-value-bind (wonp error)
        (sb-unix:unix-ioctl fd 
                            (if (< cmd (expt 2 31)) cmd (- cmd (expt 2 32)))
                            (sb-alien:alien-sap (sb-alien:addr value)))
      (unless wonp
        (error "IOCTL ~a failed: ~a" cmd (sb-impl::strerror error))))
    arg))

;; (defmacro define-ioctl (name fd cmd))

;;;_ XDG
;; ref: https://freedesktop.org/wiki/Software/xdg-user-dirs/
;; ref: https://specifications.freedesktop.org/basedir-spec/latest/
(defvar *xdg-dir-table*
  (let ((tbl (make-hash-table)))
    (mapc (lambda (x) (setf (gethash (car x) tbl) (cdr x)))
          '((:desktop . "Desktop")
            (:download . "Downloads")
            (:templates . "Templates")
            (:publicshare . "Public")
            (:documents . "Documents")
            (:music . "Music")
            (:pictures . "Pictures")
            (:videos . "Videos")
            (:data-home . ".data")
            (:config-home . ".config")
            (:state-home . ".local/state")
            (:data-dirs . (#p"/usr/local/share/" #p"/usr/share/"))
            (:config-dirs . (#P"/etc/xdg/"))
            (:cache-home . ".cache")
            (:runtime-dir)))
    tbl))

(defun xdg-path-split (str)
  "Split a colon-separated list of paths."
  (mapcar (lambda (x) (pathname (directory-path x))) (ssplit ":" str :omit-nulls t)))

(defun xdg-dir (key)
  "Like GETHASH, but second value only returns T when the value is a pathname or non-nil list."
  (multiple-value-bind (v p) (gethash key *xdg-dir-table*)
    (values v (and p (or (pathnamep v) (consp v))))))

(defun (setf xdg-dir) (v k)
  (setf (gethash k *xdg-dir-table*) v))

(defun init-xdg-dirs ()
  "Init *XDG-USER-DIRS* from environment."
  (flet ((.xdg (x) 
           (if-let ((y (or (sb-posix:getenv (format nil "XDG_~:@(~A~)" (substitute #\_ #\- (string x))))
                           (sb-posix:getenv (format nil "XDG_~:@(~A~)DEV" (substitute #\_ #\- (string x)))))))
             (case x
               ((or :data-dirs :config-dirs) (xdg-path-split y))
               (t (directory-path y)))
             (multiple-value-bind (z p) (xdg-dir x)
               (if p z (probe-file (merge-homedir-pathnames z)))))))
    (mapc
     (lambda (k)
       (when-let ((e (.xdg k)))
         (setf (xdg-dir k) e)))
     (hash-table-keys *xdg-dir-table*))
    *xdg-dir-table*))

(defmethod std/meta:init ((self (eql :xdg)) &rest args)
  (prog1 (init-xdg-dirs)
    (unless (null args)
      (std/list:doplist (k v) args
        (setf (xdg-dir k) v)))))

(defun xdg-config-directory (name)
  (when-let ((p (merge-pathnames name (xdg-dir :config-home))))
    (directory-path p)))

(defun xdg-config-file (name)
  "Attempt to find an xdg config file for NAME for searching for a match in this order: 
- ~/.config/NAMErc 
- ~/.config/NAME.*
- ~/.config/NAME/NAMErc
- ~/.config/NAME/NAME.*"
  (let ((xdg-files (std/path:directory-files (xdg-dir :config-home)))
        (our-files (std/path:directory-files (xdg-config-directory name)))
        (rc-name (concatenate 'string name "rc")))
    (flet ((.find (x y) (find x y :key 'pathname-name :test 'string-equal)))
      (or (.find rc-name xdg-files)
          (.find name xdg-files)
          (.find rc-name our-files)
          (.find name our-files)))))

;;;_ user-add
(defun user-add (name &key shell home comment base gid uid system groups (defaults t) (output t))
  (let ((useradd (probe-file "/bin/useradd")))
    (if useradd
        (sb-ext:run-program 
         useradd `(,name
                   ,@(when shell `("-s" ,shell))
                   ,@(when home `("-d" ,home))
                   ,@(when comment `("-c" ,comment))
                   ,@(when base `("-b" ,base))
                   ,@(when gid `("-g" ,gid))
                   ,@(when uid `("-u" ,uid))
                   ,@(when system '("-r"))
                   ,@(when groups (cons "-g" groups))
                   ,@(when defaults '("-D")))
         :output output)
        (error "unable to find USERADD program (/bin/useradd)"))))
                            
;;;_ group-add
(defun group-add (name &key force id users (output t))
  (let ((groupadd (probe-file "/bin/groupadd")))
    (if groupadd
        (sb-ext:run-program
         groupadd `(,name
                    ,@(when force '("-f"))
                    ,@(when id `("-i" ,id))
                    ,@(when users (cons "-i" users)))
         :output output)
        (error "unable to find GROUPADD program (/bin/groupadd)"))))

;;;_ with-directory-iterator
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

;;;_. Ambitiously portable pathname manipulations
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
  "Implicitly set current-directory to PATHSPEC using SB-POSIX:CHDIR."
  (sb-posix:chdir pathspec))

(defun call-with-directory-iterator (pathspec fun)
  (let ((dir (absolute-pathname (pathname pathspec)))
        (old-dir (current-directory)))
    (let ((dp (sb-posix:opendir dir)))
      (labels ((one-iter ()
                 (let ((dir (sb-posix:readdir dp)))
                   (unless (null-alien dir)
                     (let ((name (sb-posix:dirent-name dir)))
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
                                (make-pathname :name name))))))))))
        (declare (dynamic-extent #'one-iter))
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

(defun merge-env-pathnames (path &optional default)
  (if-let ((%default (sb-posix:getenv default)))
    (merge-pathnames path (namestring (directory-path %default)))
    path))

;;;_ StumpWM exec utils
;; from stumpwm/wrappers.lisp
(defun execv (program &rest arguments)
  "Call the system execv() function, replacing the current process image with a
new one."
  (declare (ignorable program arguments))
  (sb-alien:with-alien ((prg sb-alien:c-string program)
                        (argv (array sb-alien:c-string 256)))
    (loop
       for i in arguments
       for j below 255
       do (setf (sb-alien:deref argv j) i))
    (setf (sb-alien:deref argv (length arguments)) nil)
    (sb-alien:alien-funcall (sb-alien:extern-alien "execv" (function sb-alien:int sb-alien:c-string (* sb-alien:c-string)))
                            prg (sb-alien:cast argv (* sb-alien:c-string)))))

(defun open-pipe (&key (element-type '(unsigned-byte 8)))
  "Create a pipe and return two fd-streams. The first value is the input
stream, and the second value is the output stream."
  (multiple-value-bind (in-fd out-fd)
      (sb-posix:pipe)
    (let ((in-stream (sb-sys:make-fd-stream in-fd :input t :element-type element-type))
          (out-stream (sb-sys:make-fd-stream out-fd :output t :element-type element-type)))
      (values in-stream out-stream))))

(defun pathname-executable-p (pathname)
  "Return T if the pathname describes an executable file."
  (let ((filename (coerce (sb-ext:native-namestring pathname) 'string)))
    (and (or (pathname-name pathname)
             (pathname-type pathname))
         (sb-unix:unix-access filename sb-unix:x_ok))))

;; based on cffi version of set-signal-handler from Andrew Lyon at https://stackoverflow.com/a/10442062
;; rewritten to use SBCL's Foreign Function Interface directly by Max-Gerd Retzlaff
(defmacro set-signal-handler (signo &body body)
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien "signal" (function sb-alien:void
                                              sb-alien:int sb-alien:system-area-pointer))
    ,signo
    ;; callback function
    (sb-alien:alien-sap
     (sb-alien::alien-lambda sb-alien:void ((signum sb-alien:int))
       ,@body))))

