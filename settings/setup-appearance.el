;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq custom-safe-themes t
      visible-bell nil
      ring-bell-function 'ignore)

(setq custom-theme-directory (concat user-emacs-directory
				     "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(defun bk/default-light-theme ()
  "Default appearance for my Emacs sessions."
  (load-theme 'tsdh-light t)
  (set-frame-font "Monaco 17"))

(defun bk/default-dark-theme ()
  "Default appearance for my Emacs sessions."
  (load-theme 'default-black t)
  (set-frame-font "Monaco 17"))

(bk/default-light-theme)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
