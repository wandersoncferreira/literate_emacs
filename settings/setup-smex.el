;;; setup-smex --- SMEX
;;; Commentary:

;; Better M-x for your life

;;; Code:

(bk/install-maybe-require 'smex)

(setq smex-prompt-string "Here be dragons => ")
(smex-initialize)

(provide 'setup-smex)
;;; setup-smex.el ends here
