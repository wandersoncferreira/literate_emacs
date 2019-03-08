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
					    idle-change
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

(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 30.0)))

;; Each buffer gets its own idle-change-delay because of the
;; buffer-sensitive adjustment above.
(make-variable-buffer-local 'flycheck-idle-change-delay)

(add-hook 'flycheck-after-syntax-check-hook
          'magnars/adjust-flycheck-automatic-syntax-eagerness)


(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
