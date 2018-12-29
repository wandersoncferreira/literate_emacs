;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq custom-safe-themes t
      visible-bell nil
      ring-bell-function 'ignore)

(defun bk/default-theme ()
  "Default appearance for my Emacs sessions."
  (load-theme 'tsdh-light t)
  (set-frame-font "Monaco 15"))

(bk/default-theme)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
