;;; setup-c --- C development
;;; Commentary:

;;

;;; Code:

(bk/install-maybe 'rtags)
(bk/install-maybe 'company-rtags)

(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))

(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(setq teste-c "ativo")
(provide 'setup-c)
;;; setup-c.el ends here
