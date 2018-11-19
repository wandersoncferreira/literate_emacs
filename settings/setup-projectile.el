(bk/install-maybe-require 'projectile)

(setq projectile-completion-system 'ido
      projectile-switch-project-action 'projectile-find-file
      projectile-project-search-path '("~/captalys" "~/personal"))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-mode +1)

(provide 'setup-projectile)
