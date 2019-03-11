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

(setq whitespace-style
      '(trailing spaces lines-tail empty indentation::tab
		 indentation::space tabs newline tab-mark newline-mark))
(global-whitespace-mode 1)
(setq whitespace-display-mappings
      '(
	(space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
	(newline-mark 10 [8617 10]) ; 10 LINE FEED
	(lines-tail 10 [8617 10]) ; 10 LINE FEED
	(tab-mark 9 [8594 9] [183 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
	))

(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)


(provide 'setup-editing)
;;; setup-editing.el ends here
