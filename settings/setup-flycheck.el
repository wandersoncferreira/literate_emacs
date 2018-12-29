;;; flycheck --- custom configurations
;;; Commentary:
;;; Code:

(bk/install-maybe 'flycheck)
(bk/install-maybe 'flycheck-pos-tip)
(bk/install-maybe 'flycheck-clojure)

(require 'flycheck)
(require 'flycheck-pos-tip)
(require 'flycheck-clojure)

(global-flycheck-mode +1)

(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

(setq flycheck-check-syntax-automatically '(save
					    mode-enabled))
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(eval-after-load 'clojure-mode
  '(eval-after-load 'cider
     '(eval-after-load 'flycheck
	'(flycheck-clojure-setup))))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
