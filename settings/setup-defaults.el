;;; setup-defaults.el --- Defaults
;;; Commentary:
;;; Code:

(require 'uniquify)
(require 'flyspell)
(require 'saveplace)
(require 'ibuffer)
(require 'linum)
(require 'autorevert)

;; fix old security emacs problems
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(add-to-list 'exec-path "/Users/wandersonferreira/dotfiles/scripts")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/opt/local/bin/")

;;; mark
;; sometimes you just want to explicitly set a mark into one place
;; so you can get back to it latter with C-u SPC
(defun push-mark-no-activate ()
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring."))

(setq echo-keystrokes 0.1
      line-number-mode t
      column-number-mode t
      enable-local-variables :safe
      load-prefer-newer t
      tab-always-indent 'complete
      help-window-select t
      delete-old-versions t
      vc-make-backup-files t
      save-place-mode t
      save-place-file (expand-file-name ".places" user-emacs-directory)
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      backup-by-copying t
      create-lockfiles nil
      default-tab-width 4
      shift-select-mode nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'global-subword-mode)
(add-hook 'after-init-hook 'savehist-mode)

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

(setq ibuffer-default-sorting-mode 'major-mode)
(add-hook 'ibuffer-mode-hook (lambda nil
			       (visual-line-mode -1)))

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t
      uniquify-separator " • "
      uniquify-ignore-buffers-re "^\\*")

(put 'erase-buffer 'disabled nil)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "C-,") nil)

;; to compare the contents of two test files, use M-x ediff-files.
;; open the two files you want to compare.
;; Press | to put the two files side by side
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (quote split-window-horizontally))
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;; utf-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; change some colors for the diff-mode
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(provide 'setup-defaults)
;;; setup-defaults.el ends here
