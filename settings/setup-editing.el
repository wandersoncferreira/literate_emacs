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

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))


(global-set-key (kbd "M-\\") (lambda ()
                               (interactive)
                               (just-one-space -1)
                               (delete-horizontal-space)))

(setq split-height-threshold nil
      split-width-threshold 0)

(use-package phi-search
  :ensure t
  :bind (("C-c s s" . phi-search)
         ("C-c s r" . phi-search-backward)))

;;; while searching, following commands are available:
;;; URL: https://github.com/zk-phi/phi-search
  ;; [C-s] - phi-search-again-or-next
  ;; [C-r] - phi-search-again-or-previous
  ;; [C-v] - phi-search-scroll-up
  ;; [M-v] - phi-search-scroll-down
  ;; [C-l] - phi-search-recenter
  ;; [M-c] - phi-search-case-toggle
  ;; [C-w] - phi-search-yank-word
  ;; RET   - phi-search-complete
  ;; C-RET - phi-search-complete-at-beginning
  ;; C-c C-c phi-search-unlimit
  ;; C-g   - phi-search-abort

  (require 'phi-replace)

  (global-set-key (kbd "M-%") 'phi-replace-query)


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
