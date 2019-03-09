;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

(bk/install-maybe 'zenburn-theme)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq custom-safe-themes t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      ns-pop-up-frames nil
      use-file-dialog nil
      use-dialog-box nil
      visible-bell nil)

;;; blink the modeline on errors
(setq ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.05 nil 'invert-face 'mode-line)))

(setq custom-theme-directory (concat user-emacs-directory "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(load-theme 'zenburn t)

(or-protected
 (not (set-frame-font "Monaco 14"))
 (not (set-frame-font "Liberation Mono 15"))
 (not (set-frame-font "Ubuntu Mono 16")))

(when window-system
  (setq frame-title-format '(buffer-file-format "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;;; make zooming affect frame instead of buffers
(require 'zoom-frm)
(defun enable-zoom-one-shot-keybindings ()
  "Enable zoom-frm with only one key stroke."
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "+") 'zoom-frm-in)
     (define-key map (kbd "-") 'zoom-frm-out)
     (define-key map (kbd "0") 'zoom-frm-unzoom)
     map) t))

(defun zoom-frame-in ()
  "Zoom frame in and them only press one key to continue."
  (interactive)
  (zoom-frm-in)
  (enable-zoom-one-shot-keybindings))

(defun zoom-frame-out ()
  "Zoom frame out and them only press one key to continue."
  (interactive)
  (zoom-frm-out)
  (enable-zoom-one-shot-keybindings))


(provide 'setup-appearance)
;;; setup-appearance.el ends here
