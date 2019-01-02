;;; setup-lisp.el --- Lisp
;;; Commentary:
;;; Code:

;;; elisp go-to-definition with M-. and back again with M-,
(bk/install-maybe 'elisp-slime-nav)
(bk/install-maybe 'slime)

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (elisp-slime-nav-mode t)
				  (eldoc-mode +1)))
(diminish 'elisp-slime-nav-mode)


(setq inferior-lisp-program "ccl")
(setq slime-contribs '(slime-fancy slime-cl-indent))
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-fuzzy-completion-in-place t
      slime-enable-evaluate-in-emacs t
      slime-autodoc-use-multiline-p t)

(require 'slime)

(provide 'setup-lisp)
;;; setup-lisp.el ends here
