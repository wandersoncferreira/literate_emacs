;;; setup-defaults.el --- Defaults
;;; Commentary:
;;; Code:

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
      shift-select-mode nil
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
(use-package paren
  :config
  (add-hook 'after-init-hook #'show-paren-mode))

(use-package elec-pair
  :config
  (add-hook 'after-init-hook #'electric-pair-mode))

(use-package delsel
  :config
  (add-hook 'after-init-hook #'delete-selection-mode))

(use-package prog-mode
  :config
  (add-hook 'after-init-hook #'global-prettify-symbols-mode))

(use-package winner
  :config
  (add-hook 'after-init-hook #'winner-mode))

(use-package autorevert
  :init
  (setq global-auto-revert-non-file-buffers t
	auto-revert-verbose t)
  :config
  (add-hook 'after-init-hook #'global-auto-revert-mode))

(use-package savehist
  :init
  (setq history-length 1000)
  :config
  (add-hook 'after-init-hook #'savehist-mode))

(defalias 'cquit 'cider-quit)
(defalias 'ctest 'cider-test-run-test)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp "Query replace regexp")

(use-package tramp
  :preface
  (defun tramp-set-auto-save ()
    "Overwriting the `tramp-set-auto-save'.
The idea is to completely turn off backups for Tramp."
    (auto-save-mode -1))
  :init
  (setq tramp-default-method "ssh"
	tramp-backup-directory-alist backup-directory-alist
	tramp-auto-save-directory "~/tmp/tramp"
	tramp-chunksize 2000
	tramp-use-ssh-controlmaster-options "ssh"))


(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq save-abbrevs 'silent)
  :hook ((text-mode . abbrev-mode)
	 (prog-mode . abbrev-mode)))


(use-package recentf
  :diminish recentf-mode
  :init
  (setq-default recentf-max-saved-items 1000
		recentf-exclude '("/tmp/" "/ssh:"))
  :config
  (add-hook 'after-init-hook 'recentf-mode))

(use-package ibuffer
  :init
  (setq ibuffer-default-sorting-mode 'major-mode)
  :config
  (add-hook 'ibuffer-mode-hook (lambda nil
				 (visual-line-mode -1))))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-after-kill-buffer-p t
	uniquify-separator " • "
	uniquify-ignore-buffers-re "^\\*"))

(use-package flyspell
  :diminish flyspell-mode
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode))
  :bind (:map flyspell-mode-map
	      ("C-," . nil)))

(use-package saveplace
  :init
  (setq save-place-file (expand-file-name ".places"
					  user-emacs-directory))
  :config
  (setq-default save-place t))

(use-package simple
  :diminish visual-line-mode
  :preface
  (defun goto-line-with-feedback ()
    "Show line numbers when `goto-line' is pressed."
    (interactive)
    (unwind-protect
	(progn
	  (linum-mode +1)
	  (goto-line (read-number "Goto line: ")))
      (linum-mode -1)))
  :bind ([remap goto-line] . goto-line-with-feedback)
  :config
  (add-hook 'after-init-hook #'global-visual-line-mode))

(provide 'setup-defaults)
;;; setup-defaults.el ends here
