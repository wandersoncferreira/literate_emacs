;;; misc --- my miscellaneous
;;; Commentary:
;;; Code:

;;; install only packages
(bk/install-maybe 'htmlize)
(bk/install-maybe 'restclient)
(bk/install-maybe 'yaml-mode)
(bk/install-maybe 'dockerfile-mode)
(bk/install-maybe 'fix-word)
(bk/install-maybe 'whitespace-cleanup-mode)
(bk/install-maybe 'keyfreq)

(require 'whitespace)

(add-hook 'prog-mode-hook
	  (defun bk--add-watchwords ()
	    (font-lock-add-keywords
	     nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
		    1 font-lock-warning-face t)))))

(setq whitespace-style '(trailing lines space-before-tab
				  indentation space-after-tab)
      whitespace-line-column 100)

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(diminish 'whitespace-mode)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(diminish 'hs-minor-mode)

(global-whitespace-cleanup-mode +1)
(diminish 'whitespace-cleanup-mode)

(keyfreq-mode +1)
(keyfreq-autosave-mode +1)

(provide 'setup-misc)
;;; setup-misc.el ends here
