;;; setup-misc --- Miscellaneous
;;; Commentary:
;;; Code:

(use-package htmlize :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  (require 'ob-plantuml)
  (let ((plantuml-directory (concat user-emacs-directory "var/"))
        (plantuml-link "https://superb-dca2.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (let ((plantuml-target (concat plantuml-directory "plantuml.jar")))
      (if (not (f-exists? plantuml-target))
          (progn (message "Downloading plantuml.jar")
                 (shell-command
                  (mapconcat 'identity (list "wget" plantuml-link "-O" plantuml-target) " "))
                 (kill-buffer "*Shell Command Output*")))
      (setq org-plantuml-jar-path plantuml-target))))

(use-package change-inner :ensure t)
(use-package wgrep :ensure t)
(use-package vlf :ensure t)
(use-package neotree :ensure t)
(use-package restart-emacs :ensure t)
(use-package edit-indirect :ensure t)
(use-package helm-spotify-plus :ensure t)
(use-package graphviz-dot-mode :ensure t)
(use-package quickrun :ensure t)
(use-package windresize :ensure t)
(use-package rotate :ensure t)
(use-package discover-my-major :ensure t)

;;; kill first, ask later
(use-package viking-mode
  :ensure t
  :init
  (setq viking-greedy-kill nil
        viking-enable-region-kill t
        viking-use-expand-region-when-loaded t
        viking-kill-functions (list '(lambda ()
                                       (if (region-active-p)
                                           (kill-region (region-beginning) (region-end))
                                         (delete-char 1 t)))
                                    '(lambda ()
                                       (insert (pop kill-ring))
                                       (kill-new "")
                                       (viking-kill-word)
                                       (kill-append " " nil))
                                    'viking-kill-line-from-point
                                    'viking-kill-line
                                    'viking-kill-paragraph
                                    'viking-kill-buffer))
  :config
  (viking-global-mode))

(use-package groovy-mode :ensure t)

(use-package which-key
  :ensure t
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode +1))

(use-package restclient
  :ensure t
  :config
  (add-auto-mode 'restclient-mode "\\.restclient\\'"))

(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package rg
  :ensure t
  :config
  (rg-define-search rg-todo
    "Run ripgrep in current project searching for TODOs.
 The project root will will be determined by either common project
 packages like projectile and `find-file-in-project' or the source
 version control system."
    :format regexp
    :query (mapconcat (lambda (x) (format "(%s)" (car x)))
                      hl-todo-keyword-faces "|")
    :dir project))

(use-package ini-mode
  :ensure t
  :config
  (add-auto-mode 'ini-mode "\\.ini\\'"))

(use-package yaml-mode
  :ensure t
  :config
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))

(use-package json-mode
  :ensure t
  :config
  (add-auto-mode 'json-mode "\\.json\\'"))

(use-package disk-usage
  :ensure t
  :commands (disk-usage))

(use-package atomic-chrome
  :ensure t
  :init
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)))
  :config
  (atomic-chrome-start-server))

(use-package alert
  :ensure t
  :config
  (setq alert-default-style 'notification))

(when (require 'so-long nil :noerror)
  (global-so-long-mode 1))

(use-package webpaste
  :ensure t
  :config
  (setq webpaste-provider-priority '("ix.io" "dpaste.org")))

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

(use-package move-dup
  :ensure t
  :config
  (global-set-key [M-up] 'md/move-lines-up)
  (global-set-key [M-down] 'md/move-lines-down)
  (global-set-key [C-M-down] 'md/duplicate-down)
  (global-set-key [C-M-up] 'md/duplicate-up))

(use-package deft
  :ensure t
  :config
  (setq deft-default-extension "org")
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))
  (setq deft-extensions '("txt" "org")
        deft-directory "~/notes"
        deft-auto-save-interval 0
        deft-auto-save-interval 200
        deft-text-mode 'org-mode
        deft-recursive t))

(provide 'setup-misc)
;;; setup-misc.el ends here
