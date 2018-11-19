;; allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)


;; always display line and col numbers
(setq line-number-mode t
      column-number-mode t)

;; save a list of recent files visited
(recentf-mode +1)
(setq recentf-max-saved-items 100)

;; ibuffer
(setq ibuffer-default-sorting-mode 'major-mode)
(add-hook 'ibuffer-mode-hook (lambda nil
			       (visual-line-mode -1)))

;; save minibuffer history
(savehist-mode +1)
(setq history-length 1000)

;; undo/redo window configurations with C-c <left>/<right>
(winner-mode +1)

(setq-default indicate-empty-lines t)

;; don't break lines for me
(setq-default truncate-lines t)

;; dont be so stingy on the memory
(setq gc-cons-threshold 20000000)

;; sentences don't need double space to end
(setq-default sentence-end-double-space nil)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t
      uniquify-separator " • "
      uniquify-ignore-buffers-re "^\\*")

(setq tab-always-indent 'complete
      help-window-select t
      delete-old-versions t
      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      create-lockfiles nil
      shift-select-mode nil)

(electric-pair-mode +1)
(global-visual-line-mode +1)
(diminish 'visual-line-mode)

;; spelling
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(diminish 'flyspell-mode)

;; make backups of files, even when they are under version control
(setq vc-make-backup-files t)

;; save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;; line numbers when needed!
(defun goto-line-with-feedback ()
  (interactive)
  (unwind-protect
      (progn
	(linum-mode +1)
	(goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; utf-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; auto revert mode looks for changes to files, and updates them
(global-auto-revert-mode +1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; aliases
(defalias 'cquit 'cider-quit)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp "Query replace regexp")

(provide 'setup-defaults)
