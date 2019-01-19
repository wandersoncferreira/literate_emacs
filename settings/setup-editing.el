;;; editing.el --- help me improve
;;; Commentary:
;;; Code:

;;; expand region
(bk/install-maybe 'expand-region)
(bk/install-maybe 'jump-char)
(bk/install-maybe 'ace-jump-mode)

(require 'expand-region)
(require 'jump-char)
(require 'ace-jump-mode)

;; don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 100)

(provide 'setup-editing)
;;; setup-editing.el ends here
