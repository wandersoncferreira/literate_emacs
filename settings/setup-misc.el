;;; setup-misc --- Miscellaneous
;;; Commentary:
;;; Code:

;;; install only packages
(use-package htmlize :ensure t)
(use-package restclient :ensure t)
(use-package yaml-mode :ensure t)
(use-package graphviz-dot-mode :ensure t)
(use-package quickrun :ensure t)
(use-package windresize :ensure t)
(use-package rotate :ensure t)
(use-package discover-my-major :ensure t)
(use-package plantuml-mode :ensure t)
(use-package change-inner :ensure t)
(use-package wgrep :ensure t)
(use-package vlf :ensure t)
(use-package neotree :ensure t)

(use-package webpaste
  :ensure t
  :config
  (setq webpaste-provider-priority '("dpaste.de")))

(use-package pdf-tools
  :ensure t
  :init
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package sqlformat
  :ensure t
  :init
  (setq sqlformat-command 'pgformatter))

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
        ("http://endlessparentheses.com/atom.xml" emacs)
        ("https://www.reddit.com/r/emacs/.rss" emacs)
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

(provide 'setup-misc)
;;; setup-misc.el ends here
