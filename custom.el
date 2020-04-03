(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-enlighten-mode nil)
 '(package-selected-packages
   (quote
    (toc-org yasnippet-snippets rg modus-vivendi-theme modus-operandi-theme elfeed ace-window windresize clojure-mode-extra-font-locking avy which-key hc-zenburn-theme anti-zenburn-theme zenburn-theme git-timemachine plantuml-mode clj-refactor highlight-indentation smart-shift night-owl-theme minimal-theme warm-night-theme monochrome-theme humanoid-themes nord-theme mlso-theme gotham-theme eshell-bookmark docker docker-compose-mode dockerfile-mode fantom-theme markdown-mode smartparens flycheck-clj-kondo flycheck multiple-cursors restclient color-theme-sanityinc-tomorrow json-mode tomatinho smex projectile paredit magit cider change-inner)))
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
	   (\`
	    (((\,
	       (concat "("
		       (regexp-opt
			(quote
			 ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
			t)
		       "\\_>"))
	      1
	      (quote font-lock-variable-name-face)))))
     (cider-docker-translations
      ("/app/src" . "/home/wand/platform/seu-barriga/src")
      ("/app/test" . "/home/wand/platform/seu-barriga/test"))
     (cider-docker-translations
      ("/app/src" . "/home/wand/platform/banker/src")
      ("/app/test" . "/home/wand/platform/banker/test"))
     (cider-docker-translations
      ("/app/src" . "/home/wand/platform/register/src")))))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit aw-mode-line-face :foreground "#89C5C8" :height 2.0))))
 '(button ((t (:foreground "#89C5C8" :underline t)))))
