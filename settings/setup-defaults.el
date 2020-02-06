;;; setup-defaults.el --- Defaults
;;; Commentary:
;;; Code:

;; fix old security emacs problems
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; garbage-collect on focus-out, emacs should feel snappier
(add-hook 'focus-out-hook #'garbage-collect)

(windmove-default-keybindings)

(setq-default
 ad-redefinition-action 'accept
 help-window-select t
 select-enable-clipboard t
 indent-tabs-mode nil)

;;; auto save when losing focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(setq echo-keystrokes 0.1
      line-number-mode t
      column-number-mode t
      enable-local-variables t
      load-prefer-newer t
      tab-always-indent 'complete
      delete-old-versions t
      vc-make-backup-files t
      save-place-mode t
      save-place-file (expand-file-name ".places" user-emacs-directory)
      backup-by-copying t
      create-lockfiles nil
      shift-select-mode nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; dont "ping Germany" when typing test.de<TAB>
(setq ffap-machine-p-known 'reject)

(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'savehist-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

(use-package subword
  :diminish subword-mode
  :config
  (add-hook 'prog-mode-hook 'subword-mode))

(use-package abbrev
  :hook ((text-mode . abbrev-mode)
         (prog-mode . abbrev-mode))
  :diminish abbrev-mode
  :init
  (setq save-abbrevs 'silent))

(use-package recentf
  :init
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never
        recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "recentf"
                          "url"
                          "COMMIT_EDITMSG\\'"
                          "/tmp/"
                          "/ssh:"))
  :config
  (recentf-mode +1))

(save-place-mode +1)
(setq-default history-length 500)

(setq uniquify-after-kill-buffer-p t
      uniquify-separator " • "
      uniquify-buffer-name-style 'reverse
      uniquify-ignore-buffers-re "^\\*")

(put 'erase-buffer 'disabled nil)

(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (define-key flyspell-mode-map (kbd "C-,") nil))

(use-package winner
  :init
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"))
  :config
  (add-hook 'after-init-hook 'winner-mode))

(setq-default fill-column 70)

(use-package switch-window
  :ensure t
  :config
  (setq-default switch-window-shortcut-style 'alphabet
                switch-window-timeout nil)
  :bind
  ("C-x o" . switch-window))

;;; rearrange split windows
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other window and re-split."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(provide 'setup-defaults)
;;; setup-defaults.el ends here
