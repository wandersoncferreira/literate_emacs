(bk/install-maybe-require 'smex)

(smex-initialize)

(setq smex-prompt-string "Here be dragons => ")

(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-x") 'smex)

(provide 'setup-smex)
