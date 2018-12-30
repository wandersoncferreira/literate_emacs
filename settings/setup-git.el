;;; setup-git.el --- git
;;; Commentary:
;;; Code:

(bk/install-maybe 'magit)
(bk/install-maybe 'git-timemachine)
(bk/install-maybe 'gist)

(require 'magit)
(setq magit-no-confirm '(stage-all-changes
			 unstage-all-changes)
      magit-branch-arguments (remove "--track" magit-branch-arguments)
      magit-completing-read-function 'magit-ido-completing-read)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;; github
(bk/install-maybe 'yagist)
(bk/install-maybe 'github-clone)
(bk/install-maybe 'magithub)

(require 'magithub)
(eval-after-load 'magit
  '(progn
     (magithub-feature-autoinject t)
     (setq magithub-clone-default-directory "~/personal")))

(provide 'setup-git)
;;; setup-git.el ends here
