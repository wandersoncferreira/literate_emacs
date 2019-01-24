;;; setup-defaults.el --- Defaults
;;; Commentary:
;;; Code:

(require 'tramp)
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
      linum-format " %3d "
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      backup-by-copying t
      create-lockfiles nil
      default-tab-width 4
      shift-select-mode nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
(electric-pair-mode +1)
(show-paren-mode +1)
(delete-selection-mode +1)
(global-auto-revert-mode +1)
(semantic-mode +1)
(global-subword-mode +1)
(savehist-mode +1)

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

(provide 'setup-defaults)
;;; setup-defaults.el ends here
