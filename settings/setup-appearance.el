;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(bk/install-maybe 'pos-tip)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq custom-safe-themes t
      visible-bell nil
      ring-bell-function 'ignore)

(setq custom-theme-directory
      (concat user-emacs-directory "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(bk/install-maybe 'base16-theme)
(load-theme 'base16-onedark t)

(or-protected
 (not (set-frame-font "Monaco 15"))
 (not (set-frame-font "Liberation Mono 15"))
 (not (set-frame-font "Ubuntu Mono 16")))


(provide 'setup-appearance)
;;; setup-appearance.el ends here
