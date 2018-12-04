;;; flycheck --- custom configurations
;;; Commentary:
;;; Code:

(bk/install-maybe-require 'flycheck)

(global-flycheck-mode +1)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
