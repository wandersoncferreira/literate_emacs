;;; setup-projectile --- Projects
;;; Commentary:

;; Manage your projects

;;; Code:


(use-package projectile
  :ensure t
  :init
  (setq projectile-mode-line-function
        '(lambda () (format " Proj[%s]" (projectile-project-name)))
        projectile-completion-system 'ido
        projectile-switch-project-action 'projectile-find-file
        projectile-project-search-path '("~/platform"))
  :config
  (projectile-mode +1))


(provide 'setup-projectile)
;;; setup-projectile.el ends here
