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

(use-package emacs
  :config
  (setq x-underline-at-descent-line t
        underline-minimum-offset 1)

  (defconst bk/fixed-pitch-font "Iosevka Extended"
    "The default fixed-pitch typeface.")

  (defconst bk/fixed-pitch-font-alt "Iosevka Slab"
    "The default fixed-pitch typeface (alt).")

  (defconst bk/fixed-pitch-params ":hintstyle=hintfull"
    "Fontconfig parameters for the fixed-pitch typeface.")

  (defun bk/font-family-size (family size)
    "Set frame font to FAMILY at SIZE."
    (set-frame-font
     (concat family "-" (number-to-string size) bk/fixed-pitch-params) t t))

  (defun bk/laptop-fonts ()
    "Pass desired argument to `bk/font-sizes' for use on my small laptop monitor."
    (interactive)
    (when window-system
      (bk/font-family-size bk/fixed-pitch-font-alt 12)))

  (defun bk/desktop-fonts ()
    "Pass desired argument to `bk/font-sizes' for use on my larger desktop monitor."
    (interactive)
    (when window-system
      (bk/font-family-size bk/fixed-pitch-font 12)))

  (defun bk/fonts-per-monitor ()
    "Choose between `bk/laptop-fonts' and `bk/desktop-fonts'
based on the width of the monitor. The calculation is based on
the maximum width of my laptop's screen."
    (interactive)
    (when window-system
      (if (<= (display-pixel-width) 1366)
          (bk/laptop-fonts)
        (bk/desktop-fonts))))
  :hook (after-init . bk/fonts-per-monitor))

(use-package modus-operandi-theme
  :ensure t
  :config
  (load-theme 'modus-operandi t))

;; (set-face-attribute 'default nil :height 130)
;; (set-face-attribute 'region nil :background "lightyellow2")

(use-package popwin
  :ensure t
  :config
  (popwin-mode +1))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
