;;; Commentary:

;; Mercurial is our primary VCS - but we blur the lines by mirroring
;; our code to Git. In a few years mirroring will probably be
;; unnecessary but it's a really useful hack FTTB.

;; Our forge is based on an instance of Heptapod https://heptapod.net/
;; which is a GitLab fork. Most of the public repos are Mercurial, but
;; there are a few 'reverse-mirrors' which I maintain exclusively as
;; Git repos. Same goes for any fork I maintain - for example, Lust is
;; my fork of rustlang/rust and is just a Git repository.

;;  HACK 2023-09-15: hgcmd interface, parsers, metadata object protocols

;; https://wiki.mercurial-scm.org/Design

;; https://wiki.mercurial-scm.org/CommandServer

;; the cmdserver is the obvious solution for Skel to interact with
;; Mercurial so we'll be leaning into it right away without bothering
;; with the standard CLI. I'm unfamiliar with how this is done with
;; Git, or if it's done at all. In Mercurial's case it seems they
;; built it out of licensing issues and to avoid Python cold-start
;; penalty which aren't issues for me ATM anyway. Git is written in C
;; so doesn't suffer a cold-start hit, but it would be nice to
;; interact with repos via a similar lightweight, local, wire
;; protocol.

;; BTW It was hard to find the command to start the command server -
;; it's 'hg serve'. Here's the base shell command invoked by chg:

;; hg serve --no-profile --cmdserver chgunix --address @INITSOCKNAME --daemon-postexec chdir:/ @DIR

;;; Code:
(in-package :vc/hg)

(deferror hg-error (vc-error) () (:auto t))

(defvar *default-hg-client-buffer-size* 4096)
(defvar *hg-program* (or (cli:find-exe "rhg") (cli:find-exe "hg")))

(defun run-hg-command (cmd &optional args (output t) (wait t))
  "Run an hg command."
  (unless (listp args) (setf args (list args)))
  (setf args (mapcar 'vc/proto::namestring-or args)) ;;  TODO 2024-05-10: slow
  (sb-ext:run-program *hg-program* (push cmd args) :output output :wait wait))

(defun hg-url-p (url)
  "Return nil if URL does not look like a URL to a hg valid remote."
  (let ((url-str (if (typep url 'pathname)
                     (namestring url)
                     url)))
    (ppcre:scan '(:alternation
                  (:regex "\\.hg$")
                  (:regex "^hg://")
                  (:regex "^https://hg\\.")
                  (:regex "^hg@"))
                url-str)))

(defun hgignore (&optional (path ".hgignore"))
  (vc/proto::make-vc-ignore :path path :patterns (vc/proto::map-lines #'ppcre:create-scanner path)))

;;; Bundles
(deftype hg-bundle-type () `(member :v1 :v2))
(deftype hg-compression-engine () `(member :bzip2 :gzip :zstd))

(defvar *hg-bundlespec-options* 
  '("changegroup" "cg.version" "obsolescence" "phases" "recbranchcache" "tagsfnodescache"))

;; https://hg.guido-berhoerster.org/projects/xwrited/help/bundlespec
(defun hg-bundlespec-string-p (str)
"A hg-bundlespec string has the following formats:

<type> : The literal bundle format string is used.

<compression>-<type> : The compression engine and format are delimited by a
hyphen (\"-\").

Optional parameters follow the \"<type>\". Parameters are URI escaped
\"key=value\" pairs. Each pair is delimited by a semicolon (\";\"). The first
parameter begins after a \";\" immediately following the \"<type>\" value."
  (destructuring-bind (ct cv) (mapcar (lambda (x) (keywordicate (string-upcase x)))
                                      (ssplit #\- (car (ssplit #\; str))))
    (when (and (typep (keywordicate (string-upcase ct)) 'hg-compression-engine)
               (typep (keywordicate (string-upcase cv)) 'hg-bundle-type))
      (values ct cv))))

;;; Config
;; https://www.mercurial-scm.org/doc/hgrc.5.html
(config:defconfig hg-config (vc-config) 
  ((paths :initarg :paths)
   (ui :initarg :ui)))

(defmethod make-config ((self (eql :hg)) &key paths)
  (declare (ignore self))
  (make-instance 'hg-config :paths paths))

(defun parse-hg-uri (obj)
  "Parse a URI which may be prefixed with '[stuff]' - the uri is returned as the
first value and 'stuff' as the second."
  (etypecase obj
    (uri (values obj :hg))
    (pathname (values obj :hg))
    (string
     (if (char= (schar obj 0) #\[)
         (let ((end (position #\] obj)))
           (values (uri (subseq obj (1+ end))) (keywordicate (string-upcase (subseq obj 1 end)))))
         (values (uri obj) :hg)))))

(defun find-hgrc (&optional (root *default-pathname-defaults*) (load t))
  (when-let ((config (probe-file (merge-pathnames ".hg/hgrc" root))))
    (let ((cfg (deserialize config :toml)))
      (if load
          (let ((ret (make-config :hg)))
            (dolist (c (unwrap cfg) ret)
              (unless (null c)
                (string-case ((car c))
                  ("paths" (setf (slot-value ret 'paths) (cdr c)))
                  ("ui" (setf (slot-value ret 'ui) (cdr c)))))))
          cfg))))

(defun find-hg-bookmarks (&optional (root *default-pathname-defaults*))
  (let ((bkm (merge-pathnames ".hg/bookmarks" root)))
    (when (probe-file bkm)
      (mapcar (lambda (x) (let ((r (nreverse (ssplit #\space x)))) (cons (trim (car r)) (cadr r))))
              (lines (read-file bkm))))))

(defun find-hg-submodules (&optional (root *default-pathname-defaults*))
  (when-let ((subs (probe-file (merge-pathnames ".hgsub" root))))
    (mapcar (lambda (x) (mapcar 'trim (ssplit #\= x)))
            (lines (read-file subs)))))

(defun find-hg-requires (&optional (root *default-pathname-defaults*))
  (when-let ((reqs (probe-file (merge-pathnames ".hg/requires" root))))
    (mapcar 'trim (lines (read-file reqs)))))

;;; Repo
;; (describe (make-instance 'hg-repo))
;; https://repo.mercurial-scm.org/hg/file/tip/mercurial/interfaces/repository.py
(defclass hg-repo (vc-repo)
  ((dirstate :reader vc-dirstate) ;; working-directory
   (bookmarks :accessor vc-bookmarks :initarg :bookmarks :initform nil)
   (requires :accessor vc-requires :initform nil)))

(defmethod vc-init ((self (eql :hg)))
  (make-instance 'hg-repo :path (pathname *default-pathname-defaults*)))

(defmethod vc-init ((self hg-repo))
  (let ((path (path self)))
    (if (zerop (sb-ext:process-exit-code (run-hg-command "init" (list path))))
        path
        (hg-error "hg init failed:" path))))

(defun make-hg-repo (path &key init (update '(:bookmarks :submodules :remotes)))
  (flet ((set-requires (repo)
           (setf (vc-requires repo) 
                 (mapcar 'trim
                         (lines 
                          (with-output-to-string (s)
                            (run-hg-command "debugrequires" nil s)
                            s)))))

         (set-submodules (repo)
           (setf (vc-submodules repo) 
                 (mapcar 
                  (lambda (x) 
                    (let ((r (make-hg-repo 
                              (probe-directory (merge-pathnames (car x) path)) 
                              :update update)))
                      (unless (find "default" (vc-remotes r) :key 'name :test 'string=)
                        (push (make-vc-remote :type :hg :name "default" :url (cdr x)) (vc-remotes r)))
                      r))
                  (find-hg-submodules path))))
         (set-bookmarks (repo) (setf (vc-bookmarks repo) (find-hg-bookmarks path))))
    (let ((repo (make-instance 'hg-repo :path path)))
      (when init (vc-init repo))
      (etypecase update
        ((eql t)
         (set-requires repo)
         (set-bookmarks repo)
         (set-submodules repo))
        (cons
         (when (member :requires update) (set-requires repo))
         (when (member :bookmarks update) (set-bookmarks repo))
         (when (member :submodules update) (set-submodules repo))))
      (when-let ((cfg (find-hgrc path)))
        (setf (vc-config repo) cfg)
        (when (or (eql update t) (member :remotes update))
          (setf (vc-remotes repo) 
                (mapcar (lambda (x) 
                          (multiple-value-bind (uri type) (parse-hg-uri (cdr x))
                            (make-vc-remote :type type :url uri :name (car x))))
                        (slot-value cfg 'paths)))))
      repo)))

(defmethod vc-type ((self hg-repo)) :hg)

(defmethod vc-run ((self hg-repo) (cmd string) &rest args)
  (with-directory (path self)
    (current-directory)
    (let ((proc (run-hg-command cmd args)))
      (if (eq 0 (sb-ext:process-exit-code proc)) nil (error 'hg-error :message (format nil "hg command failed: ~A" cmd))))))

;; NOTE 2025-06-29: this needs to be compatible with skel writers
;; (defmethod print-object ((self hg-repo) stream)
;;   (print-unreadable-object (self stream)
;;     (format stream "hg-repo")
;;     (unless (zerop (length (vc-remotes self)))
;;       (format stream " ")
;;       (pprint-tabular stream (coerce (vc-remotes self) 'list) nil nil 2))))

;; (defmethod vc-init ((self list))
;;   (when-let ((form self))
;;     (make-instance 'hg-repo
;;       :path (pathname (pop form))
;;       :remotes (or (getf form :remotes) #()))))

(defmethod vc-clone ((self hg-repo) remote &key &allow-other-keys)
  (with-slots (path) self
    (sb-ext:process-exit-code (run-hg-command "clone" (list remote path)))))

(defmethod vc-pull ((self hg-repo) &optional (remote "default"))
  (vc-run self "pull" remote))

(defmethod vc-update ((self hg-repo) &optional branch)
  (vc-run self "update" branch))

(defmethod vc-push ((self hg-repo) &key (remote "default"))
  (vc-run self "push" remote))

(defmethod vc-commit ((self hg-repo) msg &key &allow-other-keys)
  (vc-run self "commit" "-m" msg))

(defmethod vc-add ((self hg-repo) &rest files)
  (vc-run self "add" files))

(defmethod vc-remove ((self hg-repo) &rest files)
  (vc-run self "remove" files))

(defmethod vc-addremove ((self hg-repo) &rest files)
  (apply 'vc-run self "addremove" files))

(defmethod vc-status ((self hg-repo) &key &allow-other-keys) (vc-run self "status"))

(defmethod vc-branch ((self hg-repo)) (vc-run self "branch"))

(defmethod vc-diff ((a hg-repo) (b hg-repo) &key &allow-other-keys) 
  (vc-run a "diff" (vc-head a) (vc-head b)))

(defmethod vc-log ((self hg-repo))
  (vc-run self "log"))

(defmethod vc-bundle ((self hg-repo) output &key rev branch base (type "zstd-v2"))
  (let ((*default-pathname-defaults* (path self))
        (args))
    (when rev
      (appendf args `("--rev" ,rev)))
    (when branch
      (appendf args `("--branch" ,branch)))
    (when base
      (appendf args `("--base" ,base)))
    (when type
      (appendf args `("--type" ,type)))
    (unless (or rev branch)
      (push "--all" args))
    (apply #'vc-run self `("bundle" ,@args ,output))
    output))

(defmethod vc-unbundle ((self hg-repo) (input pathname) &key)
  (vc-run self "unbundle" (namestring input)))

(defmethod id ((self hg-repo))
  (with-directory (path self)
    (let ((proc (run-hg-command "id" nil :stream)))
      (with-open-stream (s (sb-ext:process-output proc))
        (with-output-to-string (str)
          (loop for c = (read-char s nil)
                until (char= c #\space)
                do (write-char c str))
          (if (eq 0 (sb-ext:process-exit-code proc))
              str
              (error 'hg-error
                     :message "hg command failed: id")))))))

(defvar *fast-export-directory* (merge-pathnames ".stash/fast-export/" (user-homedir-pathname)))
(defvar *hg-fast-export-script* (merge-pathnames "hg-fast-export.sh" *fast-export-directory*))

(defun hg-fast-export (repo &optional output filter-regexp)
  "Call the hg-fast-export.sh script, converting a HG-REPO to a GIT-REPO which is
initialized at OUTPUT. Note that the repo will be 'bare' and not contain a
working directory.

FILTER-REGEXP is an optional field containing a regexp string which will be
used in the following call in the OUTPUT directory after init:

git filter-repo --invert-paths --path-regex FILTER-REGEXP --force"
  (let* ((output (ensure-directories-exist 
                  (or output (format nil "/tmp/~A" (car (last (pathname-directory (path repo))))))))
         (out-repo (make-repo output :type :git :init t)))
    (sb-ext:run-program "/bin/bash" (list 
                                     (namestring *hg-fast-export-script*)
                                     "-r" (namestring (path repo)) "-M" "default")
                        :output t
                        :directory (pathname output))
    (when filter-regexp
      (with-directory output
        (run-git-command "filter-repo" `("--invert-paths" "--path-regex" ,filter-regexp "--force"))))
    out-repo))

(defmethod vc-export ((self hg-repo) output &key filter-regexp)
  (hg-fast-export self output filter-regexp))

;;; Client
;; ref: https://wiki.mercurial-scm.org/CommandServer
(declaim (inline %make-hg-client))
(defstruct (hg-client (:constructor %make-hg-client))
  "hg-client structures contain the client connection state
  machinery and a handle to the unix socket running Mercurial command
  server."
  (pid 0 :type fixnum :read-only t)
  (pgid 0 :type fixnum)
  (cwd (sb-posix:getcwd) :type string)
  (buffer (make-array *default-hg-client-buffer-size* :element-type 'unsigned-byte :adjustable nil))
  (socket nil :type (or local-socket null))
  (caps 0 :type fixnum))

(defun make-hg-client (&optional bufsize)
  (%make-hg-client
   :buffer (make-array (or bufsize *default-hg-client-buffer-size*)
		       :element-type 'unsigned-byte
		       :adjustable nil)))

;;;; Client Protocol
;; all communication with the mercurial cmdserver is done over a
;; socket. byte order is big-endian.

;; data from server is channel-based - (channel length pair sent
;; before data) - 5 byte header total

;; on init, the server will send hello message on channel #\o. the
;; message is a signel chunk consisting of a #\Newline-separated list
;; of lines of the form:
#|
<field name>: <field data>
|#

;; fields include: capabilities, encoding, pid

#|
o
1234
<data: 1234 bytes>
|#

(defmethod vc-init ((self hg-client))
  "Initialize the hg commandserver client. This method initializes the
appropriate process IDs and a socket for communicating with the
commandserver."
  (with-slots (pid pgid socket caps) self
    (format nil "pid: ~A, pgid: ~A, socket: ~A, caps: ~A" pid pgid socket caps)))

;; TODO 2023-12-29: 
(defmethod vc-run ((self hg-client) cmd &rest args)
  (declare (ignorable args)))

;;; Low-level
(defstruct hg-nodeid id)

(defstruct hg-revlog)

(defstruct hg-manifest)

(defstruct hg-changeset id)

;;;; Dirstate

;; see also: https://wiki.mercurial-scm.org/DirstateFormatImprovementsPlan

#|
.hg/dirstate:
<p1 binhash><p2 binhash>
<list of dirstate entries>
|#

#| entry
8bit: status
32bit: mode
32bit: size
32bit: mtime
32bit: length
variable length entry (length given by the previous length field) with:
"<filename>" followed if it's a copy by: "\0<source if copy>"
|#

(defstruct dirstate-entry status mode size mtime length filename)

;; (defmethod read-dirstate-file ((self hg-repo)))
