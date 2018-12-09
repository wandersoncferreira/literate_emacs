;;; setup-git.el --- git
;;; Commentary:
;;; Code:

(bk/install-maybe-require 'magit)
(bk/install-maybe-require 'git-timemachine)

(setq-default magit-no-confirm '(stage-all-changes
                                 unstage-all-changes))

(provide 'setup-git)
;;; setup-git.el ends here
