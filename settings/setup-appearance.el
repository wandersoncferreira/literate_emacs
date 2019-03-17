;;; setup-appearance.el --- Appearance
;;; Commentary:
;;; Code:

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

(setq ring-bell-function 'ignore)

;;; theme
(bk/install-maybe 'zenburn-theme)
(load-theme 'zenburn t)
(set-face-attribute 'font-lock-comment-face nil :italic t)
(set-face-attribute 'font-lock-doc-face nil :italic t)
(zenburn-with-color-variables
  (set-face-attribute 'button nil :foreground zenburn-yellow-2)
  (set-face-attribute 'help-argument-name nil :foreground zenburn-orange :italic nil)
  (set-face-attribute 'highlight nil :background zenburn-yellow :foreground zenburn-fg-1)
  (set-face-attribute 'mode-line nil
 		      :box `(:line-width 1 :color ,zenburn-bg-1)
 		      :height 120)
  (set-face-attribute 'mode-line-inactive nil
 		      :box `(:line-width 1 :color ,zenburn-bg-05)
 		      :foreground zenburn-bg+3
 		      :height 120
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

(if (eq window-system 'ns)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; fringe
(fringe-mode '(8 . 0))
(setq-default indicate-buffer-boundaries 'left)

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
