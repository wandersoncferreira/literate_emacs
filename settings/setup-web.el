;;; setup-web --- WEB
;;; Commentary:

;; mode to deal with WEB file formatting

;;; Code:

(bk/install-maybe 'web-mode)
(bk/install-maybe 'emmet-mode)
(bk/install-maybe 'rjsx-mode)
(bk/install-maybe 'rainbow-mode)
(bk/install-maybe 'mmm-mode)
(bk/install-maybe 'sass-mode)
(bk/install-maybe 'scss-mode)
(bk/install-maybe 'css-eldoc)
(bk/install-maybe 'skewer-mode)

(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)

(dolist (hook '(css-mode-hook html-mode-hook
			      sass-mode-hook))
  (add-hook hook 'rainbow-mode))

;;; embedding in html
(eval-after-load 'mmm-vars
  '(progn
     (mmm-add-group
      'html-css
      '((css-cdata
	 :submode css-mode
	 :face mmm-code-submode-face
	 :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
	 :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
	 :insert ((?c css-tag nil @ "<style type=\"text/css\">"
		      @ "\n" _ "\n" @ "</style>" @)))
	(css
	 :submode css-mode
	 :face mmm-code-submode-face
	 :front "<style[^>]*>[ \t]*\n?"
	 :back "[ \t]*</style>"
	 :insert ((?c css-tag nil @ "<style type=\"text/css\">"
		      @ "\n" _ "\n" @ "</style>" @)))
	(css-inline
	 :submode css-mode
	 :face mmm-code-submode-face
	 :front "style=\""
	 :back "\"")))
     (dolist (mode (list 'html-mode 'nxml-mode))
       (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))))


;;; sass and scss
(require 'sass-mode)
(unless (fboundp 'scss-mode)
  (require 'scss-mode))
(setq-default scss-compile-at-save nil)

;;; use eldoc for syntax hints
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

(provide 'setup-web)
;;; setup-web.el ends here
