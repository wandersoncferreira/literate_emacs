;;; setup-git.el --- git
;;; Commentary:
;;; Code:

(bk/install-maybe 'magit)
(bk/install-maybe 'git-timemachine)
(bk/install-maybe 'yagist)
(bk/install-maybe 'browse-at-remote)
(bk/install-maybe 'magit-todos)
(bk/install-maybe 'gitconfig-mode)
(bk/install-maybe 'diff-hl)

(require 'magit)
(require 'magit-todos)

(setq magit-no-confirm '(stage-all-changes
			 unstage-all-changes))

(defun magit-quit-session ()
  "Restore the previous window configuration."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(add-hook 'magit-mode-hook 'magit-todos-mode)

(add-to-list 'auto-mode-alist '("\\.gitconfig$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))

;; diff
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'after-init-hook 'global-diff-hl-mode)

(with-eval-after-load 'diff-hl
  (define-key diff-hl-mode-map (kbd "<left-fringe> <mouse-1>")
    'diff-hl-diff-goto-hunk))

(provide 'setup-git)
;;; setup-git.el ends here
