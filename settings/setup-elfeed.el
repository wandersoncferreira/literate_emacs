;;; setup-elfeed.el --- Elfeed
;;; Commentary:
;;; Code:


(bk/install-maybe-require 'elfeed)

(setq-default elfeed-search-filter "@24-months-ago +unread")
(setq elfeed-feeds
      '(("http://lambda-the-ultimate.org/rss.xml" functional)
        ("https://byorgey.wordpress.com/feed/" functional)
        ("http://gigasquidsoftware.com/atom.xml" clojure)
        ("http://planet.emacsen.org/atom.xml" emacs)
        ("https://nullprogram.com/feed/" programming)
        ("http://feeds.feedburner.com/cognicast" clojure)
        ("http://blog.cognitect.com/blog?format=rss" clojure)
        ("http://feeds.feedburner.com/stevelosh?format=xml" clojure)
        ("https://existentialtype.wordpress.com/feed/" functional)
        ("http://planet.clojure.in/atom.xml" clojure)
        ("http://planet.lisp.org/rss20.xml" lisp)
        ("http://endlessparentheses.com/atom.xml" emacs)
        ("https://www.reddit.com/r/emacs/.rss" emacs)
        ("https://www.reddit.com/r/orgmode/.rss" emacs)
        ("http://www.blackhats.es/wordpress/?feed=rss2" emacs)
        ("http://www.howardism.org/index.xml" emacs)
        ("http://www.masteringemacs.org/feed/" emacs)
        ("http://www.scheme.dk/planet/atom.xml" scheme)
        ("http://tonsky.me/blog/atom.xml" clojure)
        ("https://danlebrero.com/feed.rss" programming)
        ("http://www.clojure.net/rss.xml" clojure)
        ("https://www.youtube.com/feeds/videos.xml?user=techguruuk" emacs)
        ("http://emacsrocks.com/atom.xml" emacs)
        ("http://emacs-fu.blogspot.com/feeds/posts/default" emacs)
        ("http://yqrashawn.com/feeds/lazyblorg-all.atom_1.0.links-only.xml" emacs)
        ))

(provide 'setup-elfeed)
;;; setup-elfeed.el ends here
