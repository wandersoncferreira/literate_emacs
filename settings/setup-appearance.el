;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defvar bk/default-font
  "-apple-Monaco-medium-normal-normal-*-15-*-*-*-m-0-iso10646-1")

(setq custom-safe-themes t
      visible-bell nil
      ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line))

      custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(defun bk/theme-tsdh-light ()
  "Let's face it... defaults are always better..."
  (interactive)
  (load-theme 'tsdh-light)
  (when init-osx?
    (set-face-attribute 'default nil :font bk/default-font)))

(defun bk/theme-default-black ()
  "Default black theme."
  (interactive)
  (load-theme 'default-black)
  (set-face-attribute 'default nil :font bk/default-font))

(bk/theme-tsdh-light)

(blink-cursor-mode -1)
(show-paren-mode +1)
(global-hl-line-mode +1)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
