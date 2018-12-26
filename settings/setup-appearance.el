;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq custom-safe-themes t
      visible-bell nil
      ring-bell-function 'ignore)

(load-theme 'tsdh-light t)
(set-default-font "Monaco 15")
(show-paren-mode +1)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
