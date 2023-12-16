;;; skel/core/vc/hg.lisp --- Skel Mercurial Version Control

;; This package holds the machinery for interacting with our primary
;; version control system: Mercurial (hg).

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
(in-package :skel/core)

(defvar *default-hg-client-buffer-size* 4096)
(defvar *hg-program* (or (find-exe "rhg") (find-exe "hg")))

;;; Mercurial
(defstruct hg-nodeid
  ""
  (id))

(defstruct hg-revlog
  "")

(defstruct hg-manifest
  "")

(defstruct hg-changeset
  ""
  (id))

(defclass hg-repo (repo)
  ((dirstate) ;; working-directory
   (bookmarks)
   (requires)))

(declaim (inline %make-hg-client))
(defstruct (hg-client (:constructor %make-hg-client))
  "hg-client structures contain the client connection state
  machinery and a handle to the unix socket running Mercurial command
  server."
  (pid 0 :type fixnum :read-only t)
  (pgid 0 :type fixnum)
  (cwd "." :type string)
  (buffer (make-array *default-hg-client-buffer-size* :element-type 'unsigned-byte :adjustable nil))
  (socket nil :type (or local-socket null))
  (caps 0 :type fixnum))

(defun make-hg-client (&optional bufsize)
  (%make-hg-client
   :buffer (make-array (or bufsize *default-hg-client-buffer-size*)
		       :element-type 'unsigned-byte
		       :adjustable nil)))

(defun run-hg-command (cmd &rest args)
  (run-program *hg-program* (push cmd args)))
