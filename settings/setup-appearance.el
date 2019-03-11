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

;;; theme
(require 'zenburn-theme)
(load-theme 'zenburn t)
(set-face-attribute 'font-lock-comment-face nil :italic t)
(set-face-attribute 'font-lock-doc-face nil :italic t)
(zenburn-with-color-variables
  (set-face-attribute 'button nil :foreground zenburn-yellow-2)
  (set-face-attribute 'help-argument-name nil :foreground zenburn-orange :italic nil)
  (set-face-attribute 'highlight nil :background zenburn-yellow :foreground zenburn-fg-1)
  (set-face-attribute 'mode-line nil
		      :box `(:line-width 1 :color ,zenburn-bg-1)
		      :height 110)
  (set-face-attribute 'mode-line-inactive nil
		      :box `(:line-width 1 :color ,zenburn-bg-05)
		      :foreground zenburn-bg+3
		      :height 110
		      )
  (set-face-attribute 'region nil
		      :background zenburn-fg-1
		      :distant-foreground 'unspecified)
  (set-face-attribute 'vertical-border nil :foreground zenburn-bg))

;; faces when we start the merge process
(require 'smerge-mode)
(zenburn-with-color-variables
  (set-face-attribute 'smerge-mine nil :background zenburn-red-2)
  (set-face-attribute 'smerge-other nil :background zenburn-green)
  (set-face-attribute 'smerge-refined-added nil :background zenburn-green-1)
  (set-face-attribute 'smerge-refined-removed nil :background zenburn-red-4))

(or-protected
 (not (set-frame-font "Monaco 14"))
 (not (set-frame-font "Liberation Mono 15"))
 (not (set-frame-font "Ubuntu Mono 16")))

(when window-system
  (setq frame-title-format '(buffer-file-format "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(if (eq window-system 'ns)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

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


;; fringe
(fringe-mode '(8 . 0))
(setq-default indicate-buffer-boundaries 'left)

;; modeline
(setq display-time-24hr-format t)
(display-time-mode t)
(size-indication-mode t)

(defun add-mode-line-dirtrack ()
  "Add track of current dir in shell mode."
  (add-to-list 'mode-line-buffer-identification
	       '(:propertize (" " default-directory " ") face dired-directory)))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;;; change some colors for the diff-mode
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


(provide 'setup-appearance)
;;; setup-appearance.el ends here
