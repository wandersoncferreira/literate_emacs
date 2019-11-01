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

(defun push-mark-no-activate ()
  "Sometimes you just want to explicitly set a mark into one place.
so you can get back to it later with `pop-to-mark-command'"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring."))

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
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
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
  :config
  (setq-default recentf-max-saved-items 1000
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
  (recentf-mode +1))


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

;;; tramp mode
(use-package tramp
  :init
  (setq tramp-default-method "ssh"
        tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory "/tmp/tramp/"
        tramp-chunksize 2000
        tramp-use-ssh-controlmaster-options "ssh"))

(use-package docker-tramp :ensure t)
(require 'docker-tramp-compat)

;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

(use-package which-func
  :defer 5
  :config
  (which-func-mode +1))

(provide 'setup-defaults)
;;; setup-defaults.el ends here
