;;; setup-ido.el --- completions
;;; Commentary:
;;; Code:

(use-package ido
  :init
  (setq ido-use-virtual-buffers t
        ido-enable-prefix nil
        ido-auto-merge-work-directories-length -1
        ido-case-fold nil
        ido-max-prospects 10
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-enable-flex-matching t)
  :config
  (ido-mode +1))

(use-package smex
  :ensure t
  :init
  (setq smex-prompt-string "Here be dragons => ")
  :config
  (smex-initialize))

(setq ido-file-extensions-order '(".clj" ".py" ".org" ".php" ".rest"))
(setq-default imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories "elpa")
(add-to-list 'ido-ignore-directories "vendor")
(add-to-list 'ido-ignore-files "\\.DS_Store")

(provide 'setup-ido)
;;; setup-ido.el ends here
