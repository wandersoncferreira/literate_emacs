;;; setup-smex --- SMEX
;;; Commentary:

;; Better M-x for your life

;;; Code:

(bk/install-maybe 'amx)
(require 'amx)

(setq amx-prompt-string "Here be dragons => ")
(amx-mode)

(provide 'setup-smex)
;;; setup-smex.el ends here
