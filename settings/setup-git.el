;;; setup-git.el --- git
;;; Commentary:
;;; Code:

(bk/install-maybe 'magit)
(bk/install-maybe 'git-timemachine)
(bk/install-maybe 'gist)

(setq-default magit-no-confirm '(stage-all-changes
				 unstage-all-changes))


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
