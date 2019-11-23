;;; setup-completion.el --- completions
;;; Commentary:
;;; Code:

(use-package ido
  :config
  (ido-mode +1)
  (ido-everywhere +1))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package flx
  :ensure t)

(provide 'setup-completion)
;;; setup-completion.el ends here
