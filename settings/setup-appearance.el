(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq bk/default-font "-apple-Monaco-medium-normal-normal-*-15-*-*-*-m-0-iso10646-1")
(setq custom-safe-themes t
      visible-bell nil
      inhibit-startup-screen t
      ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.05 nil 'invert-face 'mode-line))

      custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(load-theme 'default-black)
(when (boundp 'bk/default-font)
  (set-face-attribute 'default nil :font bk/default-font))


;;; parens config
(show-paren-mode +1)

(provide 'setup-appearance)
