;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; always display line and col numbers
(setq line-number-mode t
      column-number-mode t)

;; save a list of recent files visited
(setq-default recentf-max-saved-items 1000)
(setq-default recentf-exclude '("/tmp/" "/ssh:"))
(add-hook 'after-init-hook 'recentf-mode)

;; ibuffer
(setq ibuffer-default-sorting-mode 'major-mode)
(add-hook 'ibuffer-mode-hook (lambda nil
			       (visual-line-mode -1)))

;; save minibuffer history
(savehist-mode +1)
(setq history-length 1000)

;; eldoc is always enabled by default now..
;; just remove it from the modeline
(diminish 'eldoc-mode)

;; undo/redo window configurations with C-c <left>/<right>
(winner-mode +1)

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

;;; settings from better-defaults
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

(setq tab-always-indent 'complete
      help-window-select t
      delete-old-versions t
      backup-by-copying t
      create-lockfiles nil)

;;; real emacs knights!!
(setq shift-select-mode nil)

(electric-pair-mode +1)

;; spelling
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

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

;;; auto revert mode looks for changes to files, and updates them
(global-auto-revert-mode +1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; aliases
(defalias 'cquit 'cider-quit)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp "Query replace regexp")

(provide 'setup-defaults)
