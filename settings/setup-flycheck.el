;;; flycheck --- custom configurations
;;; Commentary:
;;; Code:

(bk/install-maybe 'flycheck)
(bk/install-maybe 'flycheck-pos-tip)

(require 'flycheck)
(require 'flycheck-pos-tip)

(global-flycheck-mode +1)

(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

(setq flycheck-check-syntax-automatically '(save
					    mode-enabled))
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
