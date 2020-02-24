;;; setup-scala --- Scala
;;; Commentary:

;;; Code:

(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :ensure t
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-mode
  :ensure t
  :hook
  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui :ensure t)
(use-package posframe :ensure t)

(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

(provide 'setup-scala)
;;; setup-scala.el ends here
