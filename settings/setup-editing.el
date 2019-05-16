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
(auto-fill-mode +1)
(setq-default fill-column 100)

;;; programming mode settings
(add-hook 'prog-mode-hook
          (defun bk--add-watchwords ()
            (font-lock-add-keywords
             nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
                    1 font-lock-warning-face t)))))

(provide 'setup-editing)
;;; setup-editing.el ends here
