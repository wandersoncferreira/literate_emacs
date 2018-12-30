;;; flycheck --- custom configurations
;;; Commentary:
;;; Code:

(bk/install-maybe 'flycheck)

(require 'flycheck)

(global-flycheck-mode +1)

(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

(setq flycheck-check-syntax-automatically '(save
					    mode-enabled))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
