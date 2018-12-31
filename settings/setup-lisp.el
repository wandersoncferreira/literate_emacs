;;; setup-lisp.el --- Lisp
;;; Commentary:
;;; Code:

;;; elisp go-to-definition with M-. and back again with M-,
(bk/install-maybe 'elisp-slime-nav)
(bk/install-maybe 'slime)
(require 'elisp-slime-nav)

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (elisp-slime-nav-mode t)
				  (eldoc-mode +1)))
(diminish 'elisp-slime-nav-mode)

(require 'slime)

(provide 'setup-lisp)
;;; setup-lisp.el ends here
