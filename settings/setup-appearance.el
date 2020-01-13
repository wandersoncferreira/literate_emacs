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

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(global-hl-line-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)
(set-face-attribute 'default nil :height 110)

;; (set-face-attribute 'region nil :background "lightyellow2")

(use-package popwin
  :ensure t
  :config
  (popwin-mode +1))

(defun bk/adjust-to-heavy-daylight ()
  (interactive)
  (load-theme 'leuven t)
  (set-frame-font "Monaco 12"))

(defun bk/adjust-to-regular-daylight ()
  (interactive)
  (load-theme 'cyberpunk t)
  (set-frame-font "Monaco 11"))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
