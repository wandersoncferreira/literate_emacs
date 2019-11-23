;;; editing.el --- help me improve
;;; Commentary:
;;; Code:

(use-package expand-region
  :ensure t
  :init
  (setq expand-region-fast-keys-enabled nil))

(use-package multiple-cursors :ensure t)
(use-package ace-jump-mode :ensure t)
(use-package fix-word :ensure t)

;; auto fill
(add-hook 'after-init-hook 'ruler-mode)
(setq-default fill-column 60)

(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'after-init-hook #'auto-fill-mode)

;;; programming mode settings
(add-hook 'prog-mode-hook
          (defun bk--add-watchwords ()
            (font-lock-add-keywords
             nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
                    1 font-lock-warning-face t)))))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config
  (add-hook 'after-init-hook
            'global-whitespace-cleanup-mode))

(use-package ini-mode
  :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode +1)
  (define-key hl-todo-mode-map (kbd "C-x t p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-x t n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-x t o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-x t i") 'hl-todo-insert))

(provide 'setup-editing)
;;; setup-editing.el ends here
