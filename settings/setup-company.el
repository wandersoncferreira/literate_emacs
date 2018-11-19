(bk/install-maybe-require 'company)

(setq company-global-modes '(not eshell-mode shell-mode
				 org-mode term-mode))
(setq company-idle-delay 0.1)
(setq company-transformers '(company-sort-by-occurrence))
(setq company-require-match 'never)
(setq company-show-numbers t)

(global-company-mode -1)

(provide 'setup-company)
