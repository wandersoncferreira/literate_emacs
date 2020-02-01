;;; setup-eshell --- Shell
;;; Commentary:

;; SHELL

;;; Code:

(require 'em-hist)
(require 'em-glob)
(require 'em-ls)
(require 'em-cmpl)

(setq eshell-save-history-on-exit t
      eshell-glob-case-insensitive t
      eshell-ls-use-colors t
      eshell-error-if-no-glob t
      eshell-cmpl-cycle-completions nil)

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
(add-hook 'eshell-mode-hook (lambda ()
                              (yas-global-mode -1)
                              (yas-minor-mode -1)))

(provide 'setup-eshell)
;;; setup-eshell.el ends here
