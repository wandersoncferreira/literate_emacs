;;; flycheck --- custom configurations
;;; Commentary:
;;; Code:

(bk/install-maybe 'flycheck)

(require 'flycheck)

(global-flycheck-mode +1)

(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(setq flycheck-display-errors-delay .3)
(setq flycheck-check-syntax-automatically '(save
					    mode-enabled))

;;; packages to install here -- linters!
;; npm install jsonlint -g  # json
;; brew install hadolint  # dockerfile
;; npm install -g eslint  # javascript
;;; you should then setup a configuration file: eslint --init
;; npm install -g jshint  # javascript
;; npm install -g markdownlint-cli  # markdown
;; gem install mdl  # markdown
;; npm install -g js-yaml

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
