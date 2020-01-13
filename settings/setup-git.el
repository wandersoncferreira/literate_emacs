;;; setup-git.el --- git
;;; Commentary:
;;; Code:

(use-package browse-at-remote :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package git-timemachine :ensure t)
(use-package gitignore-templates :ensure t)

(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-side 'left)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (diff-hl-flydiff-mode +1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode +1))

(custom-set-faces
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-insert ((t (:background "#7ccd7c"))))
 '(diff-hl-delete ((t (:background "#ee6363")))))

(use-package magit
  :ensure t
  :config
  (setq magit-no-confirm '(stage-all-changes
                           unstage-all-changes))
  (setq magit-completing-read-function 'magit-ido-completing-read))

(add-to-list 'auto-mode-alist '("\\.gitconfig$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))

;; diff
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'after-init-hook 'global-diff-hl-mode)


(provide 'setup-git)
;;; setup-git.el ends here
