;;; setup-smex --- SMEX
;;; Commentary:

;; Better M-x for your life

;;; Code:

(use-package amx
  :ensure t
  :init
  (setq amx-prompt-string "Here be dragons => ")
  :config
  (amx-mode))

(provide 'setup-smex)
;;; setup-smex.el ends here
