;;; ellis.el --- Richard's custom-file -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author: Richard Westhaver <ellis@rwest.io>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an example of what you may want to add to your custom
;; config file. Feel free to rip.

;;; Code:
(require 'inbox)

(setopt default-theme 'modus-vivendi-tinted
        company-source-directory (join-paths user-home-directory "dev/comp"))

(defvar emacs-config-source (join-paths company-source-directory "core/emacs"))

(defun edit-emacs-config (&optional src)
  (interactive (list current-prefix-arg))
  (let ((file (if src 
                  (expand-file-name "default.el" emacs-config-source) 
                user-custom-file)))
    (find-file file)))

(keymap-set user-map "e c" #'edit-emacs-config)


;; (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;; (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(repeat-mode)

(defun remember-project ()
  (interactive)
  (project-remember-project (project-current))
  project--list)

(defun remember-lab-projects ()
  (interactive)
  (project-remember-projects-under user-lab-directory t))

(keymap-global-set "C-<tab>" #'hippie-expand)
(keymap-set minibuffer-local-map "C-<tab>" #'hippie-expand)
(keymap-set user-map "p r" #'remember-project)
(keymap-set user-map "p s" #'remember-lab-projects)

(require 'sk)

(add-hook 'prog-mode-hook #'skt-mode)
(add-hook 'org-mode-hook #'skt-mode)
(add-hook 'prog-mode-hook #'company-mode-on)

(setopt skt-enable-tempo-elements t
        skt-completing-read t
        skt-delete-duplicate-marks t)

(keymap-set skt-mode-map "C-c M-b" #'tempo-backward-mark)
(keymap-set skt-mode-map "C-c M-f" #'tempo-forward-mark)
(keymap-set skt-mode-map "C-c M-a" #'tempo-complete-tag)

(use-package notmuch 
  :disabled (darwin-p)
  :ensure t
  :init
  (setopt
   mail-user-agent 'message-user-agent
   smtpmail-smtp-server "smtp.gmail.com"
   message-send-mail-function 'message-smtpmail-send-it
   smtpmail-debug-info t
   message-default-mail-headers "Cc: \nBcc: \n"
   message-kill-buffer-on-exit t
   user-mail-address "ellis@rwest.io"
   user-full-name "Richard Westhaver"
   notmuch-hello-sections '(notmuch-hello-insert-saved-searches 
                            notmuch-hello-insert-search 
                            notmuch-hello-insert-recent-searches 
                            notmuch-hello-insert-alltags)
   notmuch-show-logo nil
   notmuch-search-oldest-first nil
   notmuch-hello-hide-tags '("kill")
   notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                            (:name "unread" :query "tag:unread" :key "u")
                            (:name "new" :query "tag:new" :key "n")
                            (:name "sent" :query "tag:sent" :key "e")
                            (:name "drafts" :query "tag:draft" :key "d")
                            (:name "all mail" :query "*" :key "a")
                            (:name "todo" :query "tag:todo" :key "t")))
  :config
  ;;;###autoload
  (defun notmuch-exec-offlineimap ()
    "execute offlineimap command and tag new mail with notmuch"
    (interactive)
    (start-process-shell-command "offlineimap"
                                 "*offlineimap*"
                                 "offlineimap -o")
    (notmuch-refresh-all-buffers))

  (defun offlineimap-get-password (host port)
    (let* ((netrc (netrc-parse (expand-file-name "~/.netrc.gpg")))
           (hostentry (netrc-machine netrc host port port)))
      (when hostentry (netrc-get hostentry "password"))))

  (defun mark-as-read ()
    "mark message as read."
    (interactive)
    (notmuch-search-tag '("-new" "-unread" "-inbox")))

  (defun mark-as-todo ()
    "mark message as todo."
    (interactive)
    (mark-as-read)
    (notmuch-search-tag '("-new" "-unread" "-inbox" "+todo")))

  (defun mark-as-spam ()
    "mark message as spam."
    (interactive)
    (mark-as-read)
    (notmuch-search-tag (list "+spam")))

  (keymap-set user-map "e m" #'notmuch)
  (keymap-set user-map "e M" #'notmuch-exec-offlineimap)
  (keymap-set notmuch-search-mode-map "S" #'mark-as-spam)
  (keymap-set notmuch-search-mode-map "R" #'mark-as-read)
  (keymap-set notmuch-search-mode-map "T" #'mark-as-todo))

(use-package elfeed 
  :disabled (darwin-p)
  :ensure t
  :custom
  elfeed-feeds 
  '(("http://threesixty360.wordpress.com/feed/" blog math)
    ("http://www.50ply.com/atom.xml" blog dev)
    ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
    ("http://abstrusegoose.com/feed.xml" comic)
    ("http://accidental-art.tumblr.com/rss" image math)
    ("http://researchcenter.paloaltonetworks.com/unit42/feed/" security)
    ("http://curiousprogrammer.wordpress.com/feed/" blog dev)
    ("http://feeds.feedburner.com/amazingsuperpowers" comic)
    ("http://amitp.blogspot.com/feeds/posts/default" blog dev)
    ("http://pages.cs.wisc.edu/~psilord/blog/rssfeed.rss" blog)
    ("http://www.anticscomic.com/?feed=rss2" comic)
    ("http://feeds.feedburner.com/blogspot/TPQSS" blog dev)
    ("http://techchrunch.com/feeds" tech news)
    ("https://rss.nytimes.com/services/xml/rss/nyt/Technology.xml" tech news)
    ("https://static.fsf.org/fsforg/rss/news.xml" tech news)
    ("https://feeds.npr.org/1001/rss.xml" news)
    ("https://search.cnbc.com/rs/search/combinedcms/view.xml?partnerId=wrss01&id=10000664" fin news)
    ("https://search.cnbc.com/rs/search/combinedcms/view.xml?partnerId=wrss01&id=19854910" tech news)
    ("https://search.cnbc.com/rs/search/combinedcms/view.xml?partnerId=wrss01&id=100003114" us news)
    ("http://arxiv.org/rss/cs" cs rnd)
    ("http://arxiv.org/rss/math" math rnd)
    ("http://arxiv.org/rss/q-fin" q-fin rnd)
    ("http://arxiv.org/rss/stat" stat rnd)
    ("http://arxiv.org/rss/econ" econ rnd)
    ;; John Wiegley
    ("http://newartisans.com/rss.xml" dev blog)
    ;; comp
    ;; ("https://lab.rwest.io/comp.atom?feed_token=pHu9qwLkjy4CWJHx9rrJ" comp vc)
    ("https://www.reddit.com/r/listentothis/.rss" music reddit)
    ("https://www.ftc.gov/feeds/press-release-consumer-protection.xml" gov ftc)
    ("https://api2.fcc.gov/edocs/public/api/v1/rss/" gov fcc)
    )
  :init
  (defun yt-dl-it (url)
    "Downloads the URL in an async shell"
    (let ((default-directory "~/media/yt"))
      (async-shell-command (format "youtube-dl %s" url))))

  (defun elfeed-youtube-dl (&optional use-generic-p)
    "Youtube-DL link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (yt-dl-it it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (keymap-set elfeed-search-mode-map (kbd "d") 'elfeed-youtube-dl)

  (keymap-set user-map "e f" #'elfeed)
  (keymap-set user-map "e F" #'elfeed-update))

(use-package org-mime :disabled (darwin-p) :ensure t)

(use-package sh-script
  :disabled (darwin-p)
  :hook (sh-mode . flymake-mode))

(use-package tempo
  :custom
  tempo-interactive t
  :config
  (tempo-define-template 
   "org:readme"
   '("#+TITLE: " p n>
     "#+AUTHOR: " user-full-name " <" user-mail-address ">" n>)
   "org:readme"
   "Insert a readme.org file template.")
  (tempo-define-template "org:src"
                         '("#+begin_src " p n>
                           "#+end_src" n>)
                         "org:src"))
;;; Org Config
;; populate org-babel
(org-babel-do-load-languages
 ;; TODO 2021-10-24: bqn, apl, k
 'org-babel-load-languages '((shell . t)
			     (emacs-lisp . t)
			     (lisp . t)
			     (org . t)
			     (eshell . t)
			     (sed . t)
			     (awk . t)
			     (dot . t)
			     (js . t)
			     (C . t)
			     (python . t)
			     (lua . t)
			     (lilypond . t)))


(add-to-list 'slime-contribs 'slime-cape)
(add-hook 'slime-mode-hook #'company-mode-on)
(add-hook 'slime-repl-mode-hook #'company-mode-on)

(provide 'ellis)
;;; ellis.el ends here
