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
(setopt default-theme 'modus-vivendi-tinted)

(enable-paredit-mode)
(repeat-mode)

(use-package notmuch 
  :ensure t
  :custom 
  notmuch-init-file "~/.notmuch-config"
  mail-user-agent 'message-user-agent
  smtpmail-smtp-server "smtp.gmail.com"
  message-send-mail-function 'message-smtpmail-send-it
  smtpmail-debug-info t
  message-default-mail-headers "Cc: \nBcc: \n"
  message-kill-buffer-on-exit t
  user-mail-address "ellis@rwest.io"
  user-full-name "Richard Westhaver"
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
  (keymap-set user-map "e m" #'notmuch)
  (keymap-set user-map "e M" #'notmuch-exec-offlineimap))

(use-package elfeed :ensure t
  :custom
  elfeed-feeds 
  '(("http://threesixty360.wordpress.com/feed/" blog math)
    ("http://www.50ply.com/atom.xml" blog dev)
    ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
    ("http://abstrusegoose.com/feed.xml" comic)
    ("http://accidental-art.tumblr.com/rss" image math)
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
    )
  :init
  (keymap-set user-map "e f" #'elfeed)
  (keymap-set user-map "e F" #'elfeed-update))

(use-package org-mime :ensure t)

(use-package sh-script
  :hook (sh-mode . flymake-mode))

(provide 'ellis)
;;; ellis.el ends here
