;; highlight some words in programming modes
(add-hook 'hi-lock-mode-hook
	  (lambda nil
	    (highlight-regexp "FIXME" 'hi-red-b)
	    (highlight-regexp "TODO" 'hi-red-b)))

;; whitespace
(require 'whitespace)
(setq whitespace-style '(trailing lines space-before-tab
				  indentation space-after-tab))
(setq whitespace-line-column 100)

(bk/install-maybe-require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode +1)
(diminish 'whitespace-cleanup-mode)

;; htmlize
(bk/install-maybe-require 'htmlize)

;;; restclient
(bk/install-maybe-require 'restclient)

;; highlight escape sequences
(bk/install-maybe-require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

(provide 'setup-misc)
