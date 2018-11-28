;; highlight some words in programming modes
(add-hook 'hi-lock-mode-hook
	  (lambda nil
	    (highlight-regexp "FIXME" 'hi-red-b)
	    (highlight-regexp "TODO" 'hi-red-b)))

(require 'whitespace)
(setq whitespace-style '(trailing lines space-before-tab
				  indentation space-after-tab))
(setq whitespace-line-column 100)

;; general programming mode
(add-hook 'prog-mode-hook 'whitespace-mode)
(diminish 'whitespace-mode)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(bk/install-maybe-require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode +1)
(diminish 'whitespace-cleanup-mode)

;;; install only packages
(bk/install-maybe-require 'htmlize)
(bk/install-maybe-require 'restclient)
(provide 'setup-misc)
