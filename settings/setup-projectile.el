;;; setup-projectile --- Projects
;;; Commentary:

;; Manage your projects

;;; Code:

(bk/install-maybe 'projectile)
(require 'projectile)

(setq projectile-mode-line-function
      '(lambda () (format " Proj[%s]" (projectile-project-name))))

(setq projectile-completion-system 'ido
      projectile-enable-caching t
      projectile-file-exists-remote-cache-expire nil
      projectile-switch-project-action 'projectile-find-file
      projectile-project-search-path '("~/platform"))

(add-hook 'after-init-hook #'projectile-mode)

(provide 'setup-projectile)
;;; setup-projectile.el ends here
