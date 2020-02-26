;;; setup-snippets --- Snippets
;;; Commentary:

;; ease your life with snippets

;;; Code:

(use-package yasnippet
  :ensure t
  :init
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
        yas-verbosity 1
        yas-wrap-around-region t)
  :config
  (yas-global-mode 1)
  :bind (
         :map yas-keymap
         ("C-e" . yas/goto-end-of-active-field)
         ("C-a" . yas/goto-start-of-active-field)
         ("<return>" . yas-exit-all-snippets)
         :map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil)
         ("C-M-<return>" . yas-expand)))

(use-package yasnippet-snippets :ensure t)
(use-package clojure-snippets :ensure t)

(provide 'setup-snippets)
;;; setup-snippets.el ends here
