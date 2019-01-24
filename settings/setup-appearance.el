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
  (set-frame-font "Monaco 12"))

(defun bk/default-dark-theme ()
  "Default appearance for my Emacs sessions."
  (load-theme 'default-black t)
  (set-frame-font "Monaco 12"))

(bk/default-light-theme)

(setq initial-frame-alist
      '((background-color . "honeydew")
	(left . 50)
	(top . 50)
	(width . 106)
	(height . 60)))

(setq default-frame-alist initial-frame-alist)


(provide 'setup-appearance)
;;; setup-appearance.el ends here
