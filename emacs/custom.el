(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(default-input-method (quote latin-postfix))
 '(delete-selection-mode nil)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp spelling track truncate)))
 '(fci-rule-color "#3f1a1a")
 '(org-agenda-files (quote ("~/dotfiles/agenda/todo.org.gpg")))
 '(org-fontify-whole-heading-line t)
 '(org-publish-project-alist
   (\`
    (("default" :base-directory
      (\,
       (org2jekyll-input-directory))
      :base-extension "org" :publishing-directory
      (\,
       (org2jekyll-output-directory))
      :publishing-function org-html-publish-to-html :headline-levels 4 :section-numbers nil :with-toc nil :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>" :html-preamble t :recursive t :make-index t :html-extension "html" :body-only t)
     ("post" :base-directory
      (\,
       (org2jekyll-input-directory))
      :base-extension "org" :publishing-directory
      (\,
       (org2jekyll-output-directory org2jekyll-jekyll-posts-dir))
      :publishing-function org-html-publish-to-html :headline-levels 4 :section-numbers nil :with-toc nil :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>" :html-preamble t :recursive t :make-index t :html-extension "html" :body-only t)
     ("images" :base-directory
      (\,
       (org2jekyll-input-directory "images"))
      :base-extension "jpg\\|gif\\|png" :publishing-directory
      (\,
       (org2jekyll-output-directory "images"))
      :publishing-function org-publish-attachment :recursive t)
     ("js" :base-directory
      (\,
       (org2jekyll-input-directory "js"))
      :base-extension "js" :publishing-directory
      (\,
       (org2jekyll-output-directory "js"))
      :publishing-function org-publish-attachment :recursive t)
     ("css" :base-directory
      (\,
       (org2jekyll-input-directory "css"))
      :base-extension "css\\|el" :publishing-directory
      (\,
       (org2jekyll-output-directory "css"))
      :publishing-function org-publish-attachment :recursive t)
     ("web" :components
      ("images" "js" "css")))))
 '(org2jekyll-blog-author "Wanderson Ferreira" nil (org2jekyll))
 '(org2jekyll-jekyll-directory (expand-file-name "~/wandersoncferreira.github.io") nil (org2jekyll))
 '(org2jekyll-jekyll-drafts-dir "" nil (org2jekyll))
 '(org2jekyll-jekyll-posts-dir "_posts/" nil (org2jekyll))
 '(org2jekyll-source-directory (expand-file-name "~/blogging") nil (org2jekyll))
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow smartparens smart-mode-line messages-are-flowing ess org-alert langtool kotlin-mode sx pomidor super-save csv-mode dired-toggle github-pullrequest yasnippet-snippets whitespace-cleanup-mode volatile-highlights use-package uptimes undo-tree typo try tldr string-inflection sqlup-mode speed-type smex smartscan shackle restclient restart-emacs ranger pythonic projectile pretty-mode paradox ox-reveal ox-pandoc org2jekyll org-plus-contrib org-download org-bullets ob-go multiple-cursors move-text miniedit markdown-preview-mode magit lorem-ipsum json-mode insert-shebang indent-tools imenu-anywhere highlight-numbers helm-spotify-plus google-translate google-this golden-ratio go-stacktracer go-playground go-guru go-gopath go-eldoc go-direx go-add-tags git-timemachine gist flyspell-correct flycheck fixmee fix-word fill-column-indicator expand-region exec-path-from-shell esup epresent elpy elfeed electric-operator edit-indirect dumb-jump diredfl diminish diff-hl deft counsel company-go company-flx cloc bug-hunter boxquote beacon base16-theme auto-compile artbollocks-mode anzu ace-link)))
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c")))))
