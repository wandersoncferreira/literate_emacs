;;; setup-completion.el --- completions
;;; Commentary:
;;; Code:

(use-package ido
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-virtual-buffers t
        ido-use-filename-at-point nil
        ido-max-prospects 10)
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

(provide 'setup-completion)
;;; setup-completion.el ends here
