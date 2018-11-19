(bk/install-maybe-require 'paredit)
(diminish 'paredit-mode)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

;;; makes paredit-mode work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)


;;; elisp go-to-definition with M-. and back again with M-,
(bk/install-maybe-require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (elisp-slime-nav-mode t)
				  (eldoc-mode +1)))
(diminish 'elisp-slime-nav-mode)

(provide 'setup-lisp)
