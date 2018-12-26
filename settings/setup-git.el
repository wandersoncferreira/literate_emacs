;;; setup-git.el --- git
;;; Commentary:
;;; Code:

(bk/install-maybe 'magit)
(bk/install-maybe 'git-timemachine)

(setq-default magit-no-confirm '(stage-all-changes
				 unstage-all-changes))

(provide 'setup-git)
;;; setup-git.el ends here
