;;; util.lisp --- Disk Utilities

;; 

;;; Code:
(in-package :io/disk)

;;; Unix Mntent
(define-alien-type mntent
    (struct mntent
      (mnt-fsname c-string)
      (mnt-dir c-string)
      (mnt-type c-string)
      (mnt-opts c-string)
      (mnt-freq int)
      (mnt-passno int)))

(define-alien-routine setmntent (* mntent) (filename c-string) (type c-string))
(define-alien-routine getmntent (* mntent) (stream (* t)))
(define-alien-routine endmntent int (stream (* t)))

(define-constant +option-separator+    "," :test #'string=)

(define-constant +suboption-separator+ "=" :test #'string=)

(define-condition open-file-failed (error)
  ((file-path
    :initarg :file-path
    :reader  file-path))
  (:report
   (lambda (condition stream)
     (format stream
             "Can not get mount filesystem information: unable to open file ~a"
             (file-path condition))))
  (:documentation "Length error"))

(defun mntent-all-infos (&optional (mount-info-file "/etc/mtab"))
  (let ((root-info (setmntent mount-info-file "r"))
        (infos '()))
    (if (not (null-alien root-info))
        (labels ((get-info ()
                   (let ((info (deref (getmntent root-info))))
                     (if (not (null-alien info))
                         (push info infos)
                         (get-info))
                         infos)))
          (unwind-protect (get-info)
            (endmntent root-info)))
        (error 'open-file-failed
               :file-path mount-info-file))))

(defun mntent-info (mtab plist-key looking-for-value)
  (let ((all-infos (mntent-all-infos mtab)))
    (find-if (lambda (a)
               (when-let ((value-found (slot a plist-key)))
                 (string= value-found looking-for-value)))
             all-infos)))

(declaim (inline all-infos))
(defun all-infos (&optional (mount-info-file "/etc/mtab"))
  (mntent-all-infos mount-info-file))

(defun mountpoint-get (mount-info-file mountpoint key)
  (when-let ((infos (mntent-info mount-info-file 'mnt-dir mountpoint)))
    (slot infos key)))

(defun mountpoint-device (mountpoint &optional (mount-info-file "/etc/mtab"))
  (mountpoint-get mount-info-file mountpoint 'mnt-fsname))

(defun mountpoint-fstype (mountpoint &optional (mount-info-file "/etc/mtab"))
  (mountpoint-get mount-info-file mountpoint 'mnt-type))

(defun mountpoint-options (mountpoint &optional (mount-info-file "/etc/mtab"))
  (let* ((raw            (mountpoint-get mount-info-file mountpoint 'mnt-opts))
         (comma-splitted (cl-ppcre:split +option-separator+ raw)))
    (loop for i in comma-splitted collect
         (if (cl-ppcre:scan  +suboption-separator+ i)
             (cl-ppcre:split +suboption-separator+ i)
             i))))

;;; Unix Statvfs
(define-alien-type fsblkcnt-t unsigned-long)
(define-alien-type fsfilcnt-t unsigned-long)

(define-alien-type statvfs
    (struct statvfs
      (bsize unsigned-long)
      (frsize unsigned-long)
      (blocks fsblkcnt-t)
      (bfree fsblkcnt-t)
      (bavail fsblkcnt-t)
      (files fsfilcnt-t)
      (ffree fsfilcnt-t)
      (favail fsfilcnt-t)
      (fsig unsigned-long)
      (flag unsigned-long)
      (namemax unsigned-long)))

;; (constant (st-rdonly "ST_RDONLY"))
;; (constant (st-nosuid "ST_NOSUID"))

(sb-alien:define-alien-routine ("statvfs" %statvfs) sb-alien:int
  (path sb-alien:c-string)
  (buf (* statvfs)))

(defun statvfs (path)
  (with-alien ((buf (* statvfs) (make-alien statvfs)))
    (%statvfs path buf)
    (sb-ext:finalize buf (lambda () (free-alien buf)))
    (with-alien-slots (bsize frsize blocks bfree bavail files ffree favail fsig flag namemax) buf
      (values bsize frsize blocks bfree bavail files
              ffree favail fsig flag namemax))))

;;; Disk Info
(defun disk-space (path &optional human-readable-p)
  "Disk space information include total/free/available space."
  (multiple-value-bind (bsize frsize blocks bfree bavail files
                        ffree favail fsig flag namemax)
      (statvfs path)
      (declare (ignore bsize files ffree favail fsig flag namemax))
      (if human-readable-p
          (values (human-readable-size (* frsize blocks))
                  (human-readable-size (* frsize bfree))
                  (human-readable-size (* frsize bavail)))
          (values (* frsize blocks) (* frsize bfree) (* frsize bavail)))))

(defun disk-total-space (path &optional human-readable-p)
  "Disk total space."
  (multiple-value-bind (bsize frsize blocks bfree bavail files
                              ffree favail fsig flag namemax)
      (statvfs path)
    (declare (ignore bsize bfree bavail files ffree favail fsig flag namemax))
    (if human-readable-p
        (human-readable-size (* frsize blocks))
        (* frsize blocks))))

(defun disk-free-space (path &optional human-readable-p)
  "Disk free space."
  (multiple-value-bind (bsize frsize blocks bfree bavail files
                              ffree favail fsig flag namemax)
      (statvfs path)
    (declare (ignore bsize blocks bavail files ffree favail fsig flag namemax))
    (if human-readable-p
        (human-readable-size (* frsize bfree))
        (* frsize bfree))))

(defun disk-available-space (path &optional human-readable-p)
  "Disk available space."
  (multiple-value-bind (bsize frsize blocks bfree bavail files
                              ffree favail fsig flag namemax)
      (statvfs path)
    (declare (ignore bsize blocks bfree files ffree favail fsig flag namemax))
    (if human-readable-p
        (human-readable-size (* frsize bavail))
        (* frsize bavail))))

;;; Commands
(defun list-disks ()
  "List all physical disk use command line tool df. note: size in KB."
  (let ((disk-info-string (with-output-to-string (stream)
                            (sb-ext:run-program
                             "/bin/sh"
                             #+linux
                             '("-c" "/bin/df" "-P" "|" "grep" "^/dev")
                             #+bsd
                             '("-c" "/bin/df" "-k" "|" "grep" "^/dev")
                             :output stream))))
    (flatten
     (loop for disk-info in (ppcre:split "\\n" disk-info-string)
           collect
              #+linux
              (ppcre:register-groups-bind (filesystem size used available use-percent mounted-on)
                  ("^(.+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)%\\s+(.+)$"
                   disk-info)
                (declare (ignore filesystem size used available use-percent))
                (string-trim '(#\Space) mounted-on))
           ;; for Mac OS X
              #+bsd
              (ppcre:register-groups-bind (filesystem size used available use-percent
                                                      iused ifree iuse-percent mounted-on)
                  ("^(.+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)%\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)%\\s+(.+)$"
                   disk-info)
                (declare (ignore filesystem size used available use-percent
                                 iused ifree iuse-percent))
                (string-trim '(#\Space) mounted-on))))))

(defun disk-info (disk &optional human-readable-p)
  (multiple-value-bind (total free available)
      (disk-space disk)
    (if human-readable-p
        (list :disk disk
              :total (human-readable-size total)
              :free (human-readable-size free)
              :available (human-readable-size available)
              ;; :used (truncate (/ (* (- total available) 100) total))
              )
        (list :disk disk
              :total total
              :free free
              :available available
              ;; :used (truncate (/ (* (- total available) 100) total))
              ))))

(defun list-disk-info (&optional human-readable-p)
  "List disk information. example result: 
\(\(:DISK \"/\" :TOTAL 19993329664 :FREE 6154420224 :AVAILABLE 6154420224
  :USE-PERCENT 69)
 \(:DISK \"/mnt\" :TOTAL 21136445440 :FREE 2048335872 :AVAILABLE 974667776
  :USE-PERCENT 95))

\(\(:DISK \"/\" :TOTAL \"18.62 GB\" :FREE \"5.73 GB\" :AVAILABLE \"5.73 GB\" :USE-PERCENT
  69)
 \(:DISK \"/mnt\" :TOTAL \"19.68 GB\" :FREE \"1.91 GB\" :AVAILABLE \"929.52 MB\"
  :USE-PERCENT 95))"
  (loop for disk in (list-disks)
     collect (disk-info disk human-readable-p)))
