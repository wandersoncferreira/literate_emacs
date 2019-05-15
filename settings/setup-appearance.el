;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; disable unnecessary minor modes
(tooltip-mode -1)
(blink-cursor-mode -1)

(setq custom-safe-themes t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      ns-pop-up-frames nil
      use-file-dialog nil
      use-dialog-box nil
      visible-bell nil
      ring-bell-function 'ignore)

;;; theme
(use-package rebecca-theme
  :ensure t
  :config
  (load-theme 'rebecca t)
  (or-protected
   (not (set-frame-font "Monaco 15"))
   (not (set-frame-font "Liberation Mono 15"))
   (not (set-frame-font "Ubuntu Mono 15"))))

;; fringe
(fringe-mode '(8 . 0))
(setq-default indicate-buffer-boundaries 'left)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
