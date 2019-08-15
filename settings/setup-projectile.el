;;; setup-projectile --- Projects
;;; Commentary:

;; Manage your projects

;;; Code:

(use-package counsel-projectile
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-mode-line-prefix ""
        projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-find-file)
  :config
  (add-to-list 'projectile-other-file-alist '("clj" "py" "sql" "org"))
  (add-to-list 'projectile-other-file-alist '("org" "clj"))
  (defun projectile-short-mode-line ()
    "Short version of the default projectile mode line."
    (format " P[%s]" (projectile-project-name)))
  (setq projectile-mode-line-function 'projectile-short-mode-line)
  (setq projectile-enable-caching nil)
  (counsel-projectile-mode +1))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
