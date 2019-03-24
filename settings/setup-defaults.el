;;; setup-defaults.el --- Defaults
;;; Commentary:
;;; Code:

;; fix old security emacs problems
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; garbage-collect on focus-out, emacs should feel snappier
(add-hook 'focus-out-hook #'garbage-collect)

(add-to-list 'exec-path "/Users/wandersonferreira/dotfiles/scripts")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/opt/local/bin/")

(defun push-mark-no-activate ()
  "Sometimes you just want to explicitly set a mark into one place.
so you can get back to it later with `pop-to-mark-command'"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring."))

(setq-default
 ad-redefinition-action 'accept           ; silence warnings for redefinition
 auto-window-vscroll nil                  ; lighten vertical scroll
 cursor-in-non-selected-windows t         ; hide the cursor in inactive windows
 delete-by-moving-to-trash t              ; delete files to trash
 help-window-select t                     ; focus new help window when opened
 left-margin-width 1 right-margin-width 1 ; add left and right margin
 ns-use-srgb-colorspace nil               ; don't use sRGB colors
 select-enable-clipboard t                ; merge system's and emacs' clipboard
 window-combination-resize t              ; resize windows proportionally
 indent-tabs-mode nil
 )

(setq echo-keystrokes 0.1
      line-number-mode t
      column-number-mode t
      enable-local-variables :safe
      load-prefer-newer t
      tab-always-indent 'complete
      delete-old-versions t
      vc-make-backup-files t
      save-place-mode t
      save-place-file (expand-file-name ".places" user-emacs-directory)
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      backup-by-copying t
      create-lockfiles nil
      shift-select-mode nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'savehist-mode)

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(defun tramp-set-auto-save ()
  "Overwriting the `tramp-set-auto-save'.
The idea is to completely turn off backups for Tramp."
  (auto-save-mode -1))

(setq save-abbrevs 'silent)
(add-hook 'text-mode-hook #'abbrev-mode)
(add-hook 'prog-mode-hook #'abbrev-mode)

(setq-default recentf-max-saved-items 1000
              recentf-exclude '("/tmp/" "/ssh:"))
(add-hook 'after-init-hook 'recentf-mode)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t
      uniquify-separator " • "
      uniquify-ignore-buffers-re "^\\*")

(put 'erase-buffer 'disabled nil)

(require 'flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "C-,") nil)

;; to compare the contents of two test files, use M-x ediff-files.
;; open the two files you want to compare.
;; Press | to put the two files side by side
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (quote split-window-horizontally))

;;; utf-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(provide 'setup-defaults)
;;; setup-defaults.el ends here
