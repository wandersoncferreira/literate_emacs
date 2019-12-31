;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (fboundp 'tooltip-mode) (fboundp 'x-show-tip) (tooltip-mode -1))
(and (fboundp 'blink-cursor-mode) (blink-cursor-mode (- (*) (*) (*))))

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

(or-protected
 (not (set-frame-font "Monaco 11")))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/theme 'light)
  :config
  (sml/setup))

 (set-face-attribute 'region nil :background "lightyellow2")

(defun bk/adjust-to-heavy-daylight ()
  (interactive)
  (load-theme 'leuven t)
  (set-frame-font "Monaco 12"))

(defun bk/adjust-to-regular-daylight ()
  (interactive)
  (load-theme 'cyberpunk t)
  (set-frame-font "Monaco 11"))

(use-package page-break-lines
  :ensure t
  :init
  (global-page-break-lines-mode))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
