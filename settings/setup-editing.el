;;; editing.el --- help me improve
;;; Commentary:
;;; Code:

;;; expand region
(bk/install-maybe-require 'expand-region)

;; don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;;; navigate by char
(bk/install-maybe-require 'jump-char)
(bk/install-maybe-require 'ace-jump-mode)

(provide 'setup-editing)
;;; setup-editing.el ends here
