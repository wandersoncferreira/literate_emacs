;;; setup-defaults.el --- Defaults
;;; Commentary:
;;; Code:

(require 'tramp)
(require 'uniquify)
(require 'flyspell)
(require 'saveplace)
(require 'ibuffer)

;; fix old security emacs problems
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(setq save-interprogram-paste-before-kill t
      require-final-newline t
      echo-keystrokes 0.1
      line-number-mode t
      column-number-mode t
      enable-local-variables :safe
      load-prefer-newer t
      tab-always-indent 'complete
      help-window-select t
      delete-old-versions t
      backup-by-copying t
      create-lockfiles nil
      history-length 1000
      shift-select-mode nil
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
(electric-pair-mode +1)
(delete-selection-mode +1)
(global-auto-revert-mode +1)
(savehist-mode +1)

(defalias 'cquit 'cider-quit)
(defalias 'ctest 'cider-test-run-test)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp "Query replace regexp")

;; tramp
(defun tramp-set-auto-save ()
  "Overwriting the `tramp-set-auto-save'.
The idea is to completely turn off backups for Tramp."
  (auto-save-mode -1))

(setq save-abbrevs 'silent)
(add-hook 'text-mode-hook #'abbrev-mode)
(add-hook 'prog-mode-hook #'abbrev-mode)
(diminish 'abbrev-mode)

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

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "C-,") nil)
(diminish 'flyspell-mode)

(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(defun goto-line-with-feedback ()
  "Show line numbers when `goto-line' is pressed."
  (interactive)
  (unwind-protect
      (progn
	(linum-mode +1)
	(goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(provide 'setup-defaults)
;;; setup-defaults.el ends here
