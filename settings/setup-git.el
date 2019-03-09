;;; setup-git.el --- git
;;; Commentary:
;;; Code:

(bk/install-maybe 'magit)
(bk/install-maybe 'git-timemachine)
(bk/install-maybe 'yagist)
(bk/install-maybe 'browse-at-remote)
(bk/install-maybe 'magit-todos)
(bk/install-maybe 'gitconfig-mode)

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

(provide 'setup-git)
;;; setup-git.el ends here
