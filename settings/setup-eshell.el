;;; setup-eshell --- Shell
;;; Commentary:

;; SHELL

;;; Code:

(require 'em-hist)
(require 'em-glob)
(require 'em-ls)
(require 'em-cmpl)
(require 'em-term)

(setq eshell-save-history-on-exit t
      eshell-scroll-to-bottom-on-input 'all
      eshell-hist-ignoredups t
      eshell-glob-case-insensitive t
      eshell-ls-use-colors t
      eshell-error-if-no-glob t
      eshell-cmpl-cycle-completions nil
      eshell-prefer-lisp-functions nil
      eshell-destroy-buffer-when-process-dies t)

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌─[" 'face `(:foreground "#5317ac"))
         (propertize (user-login-name) 'face `(:foreground "red"))
         (propertize "@" 'face `(:foreground "#5317ac"))
         (propertize (system-name) 'face `(:foreground "lightblue"))
         (propertize "]──[" 'face `(:foreground "#5317ac"))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "lightblue"))
         (propertize "]──[" 'face `(:foreground "#5317ac"))
         (propertize (concat (eshell/pwd)) 'face `(:foreground "black"))
         (propertize "]\n" 'face `(:foreground "#5317ac"))
         (propertize "└─>" 'face `(:foreground "#5317ac"))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "#5317ac"))
         )))

(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))

(defun eshell/close ()
  (delete-window))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "emacs" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            (eshell/alias "d" "dried $1")))

(setq eshell-visual-commands '("htop" "vi" "screen" "top" "less"
                               "more" "lynx" "ncftp" "pine" "tmux"
                               "tin" "trn" "elm" "vim"))

(setq eshell-visual-subcommands '("git" "log" "diff" "show" "ssh"))

(setenv "PAGER" "cat")

(use-package eshell-autojump
  :ensure t)

(add-hook 'eshell-mode-hook #'abbrev-mode)
(add-hook 'eshell-mode-hook (lambda ()
                              (yas-global-mode -1)
                              (yas-minor-mode -1)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-M-a") 'eshell-previous-prompt)
            (define-key eshell-mode-map (kbd "C-M-e") 'eshell-next-prompt)))

(defun eshell-pop--kill-and-delete-window ()
  (unless (one-window-p)
    (delete-window)))

(add-hook 'eshell-exit-hook 'eshell-pop--kill-and-delete-window)

(provide 'setup-eshell)
;;; setup-eshell.el ends here
