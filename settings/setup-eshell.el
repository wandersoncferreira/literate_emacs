;;; setup-eshell --- Shell
;;; Commentary:

;; SHELL

;;; Code:

(setq eshell-save-history-on-exit t
      eshell-glob-case-insensitive t
      eshell-ls-use-colors t
      eshell-error-if-no-glob t
      eshell-cmpl-cycle-completions nil)

;; some commands require a proper terminal to run and eshell can't handle
(require 'em-term)
(nconc eshell-visual-commands
       '("bower" "htop" "docker" "top" "ssh"
         "ranger" "npm" "tail" "docker-compose"))

(nconc eshell-visual-subcommands '(("docker" "build")
                                   ("docker-compose" "up")
                                   ("git" "log" "diff" "show" "blame")
                                   ("npm" "init" "install")))

(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "emacs" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            (eshell/alias "d" "dried $1")))

(add-hook 'eshell-mode-hook #'abbrev-mode)

(provide 'setup-eshell)
;;; setup-eshell.el ends here
