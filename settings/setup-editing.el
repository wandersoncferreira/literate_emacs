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

(provide 'setup-editing)
;;; setup-editing.el ends here
