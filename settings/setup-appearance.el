;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; disable unnecessary minor modes
(tooltip-mode -1)

(setq custom-safe-themes t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      use-file-dialog nil
      use-dialog-box nil
      visible-bell nil
      ring-bell-function 'ignore)

(use-package rebecca-theme
  :ensure t
  :config
  (or-protected
   (not (set-frame-font "Source Code Pro 14"))
   (not (set-frame-font "Inconsolata 12"))))

;; highlight current line
(global-hl-line-mode +1)

;; line numbers in the buffer
(use-package linum
  :init
  (setq linum-format " %3d ")
  :config
  (global-linum-mode +1))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
