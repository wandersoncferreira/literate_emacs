;;; editing.el --- help me improve
;;; Commentary:
;;; Code:

;;; expand region
(bk/install-maybe 'expand-region)
(bk/install-maybe 'ace-jump-mode)
(bk/install-maybe 'fix-word)
(bk/install-maybe 'whitespace-cleanup-mode)
(bk/install-maybe 'browse-kill-ring)

(require 'expand-region)
(require 'ace-jump-mode)
(require 'whitespace)

;; don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; auto fill
(auto-fill-mode +1)
(setq-default fill-column 100)

;;; programming mode settings
(add-hook 'prog-mode-hook
	  (defun bk--add-watchwords ()
	    (font-lock-add-keywords
	     nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
		    1 font-lock-warning-face t)))))

(setq whitespace-style '(trailing lines space-before-tab
				  indentation space-after-tab)
      whitespace-line-column 100)

(whitespace-mode +1)
(prettify-symbols-mode +1)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(whitespace-cleanup-mode +1)


(provide 'setup-editing)
;;; setup-editing.el ends here
