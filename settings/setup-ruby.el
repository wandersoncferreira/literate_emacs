;;; setup-ruby --- Ruby dev
;;; Commentary:

;;

;;; Code:

(setq-default ruby-use-encoding-map nil
	      ruby-insert-encoding-magic-comment nil)

;; robe
(bk/install-maybe 'robe)
(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))

(with-eval-after-load 'company
  (push 'company-robe company-backends))

(provide 'setup-ruby)
;;; setup-ruby.el ends here
