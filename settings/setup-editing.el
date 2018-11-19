;;; expand region
(bk/install-maybe-require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; show expand-region command used
(setq er--show-expansion-message t)

;;; eldoc
(diminish 'eldoc-mode)

;;; navigate by char
(bk/install-maybe-require 'jump-char)
(bk/install-maybe-require 'ace-jump-mode)

;; kill ring
(bk/install-maybe-require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

(provide 'setup-editing)
