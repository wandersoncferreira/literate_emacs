;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

(setq initial-scratch-message "")

(or-protected
 (not (set-frame-font "Monaco 10"))
 (not (set-frame-font "Source Code Pro 14"))
 (not (set-frame-font "Inconsolata 12")))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1))

(set-face-attribute 'region nil :background "lightyellow2")

(provide 'setup-appearance)
;;; setup-appearance.el ends here
