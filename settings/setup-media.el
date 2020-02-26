;;; setup-media.el --- summary -*- lexical-binding: t -*-

;; Author: Wanderson
;; Keywords: spotify, RSS feed


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(use-package helm-spotify-plus :ensure t)

(global-set-key (kbd "C-c m s") 'helm-spotify-plus)
(global-set-key (kbd "C-c m f") 'helm-spotify-plus-next)
(global-set-key (kbd "C-c m b") 'helm-spotify-plus-previous)
(global-set-key (kbd "C-c m p") 'helm-spotify-plus-play)
(global-set-key (kbd "C-c m g") 'helm-spotify-plus-pause)

(use-package elfeed
  :ensure t
  :init
  (setq-default elfeed-search-filter "@24-months-ago +unread")
  (setq elfeed-feeds
        '(("http://lambda-the-ultimate.org/rss.xml" functional)
          ("https://byorgey.wordpress.com/feed/" functional)
          ("http://gigasquidsoftware.com/atom.xml" clojure)
          ("http://planet.emacsen.org/atom.xml" emacs)
          ("https://gigasquidsoftware.com/atom.xml" clojure)
          ("https://lambdaisland.com/feeds/blog.atom" clojure)
          ("https://nullprogram.com/feed/" programming)
          ("http://feeds.feedburner.com/cognicast" clojure)
          ("http://feeds2.feedburner.com/StuartSierra" clojure)
          ("http://feeds.feedburner.com/Juxt" clojure)
          ("http://blog.cognitect.com/blog?format=rss" clojure)
          ("http://feeds.feedburner.com/stevelosh?format=xml" clojure)
          ("https://existentialtype.wordpress.com/feed/" functional)
          ("http://planet.clojure.in/atom.xml" clojure)
          ("http://insideclojure.org/feed.xml" clojure)
          ("https://yogthos.net/feed.xml" clojure)
          ("http://endlessparentheses.com/atom.xml" emacs)
          ("http://www.blackhats.es/wordpress/?feed=rss2" emacs)
          ("http://www.howardism.org/index.xml" emacs)
          ("http://www.masteringemacs.org/feed/" emacs)
          ("http://tonsky.me/blog/atom.xml" clojure)
          ("https://danlebrero.com/feed.rss" programming)
          ("http://www.clojure.net/rss.xml" clojure)
          ("https://www.youtube.com/feeds/videos.xml?user=techguruuk" emacs)
          ("http://emacsrocks.com/atom.xml" emacs)
          ("http://emacs-fu.blogspot.com/feeds/posts/default" emacs)
          ("http://yqrashawn.com/feeds/lazyblorg-all.atom_1.0.links-only.xml" emacs)
          )))

(provide 'setup-media)

;;; setup-media.el ends here
