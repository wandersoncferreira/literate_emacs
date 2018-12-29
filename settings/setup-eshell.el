;;; setup-eshell --- Shell
;;; Commentary:

;; SHELL

;;; Code:

(require 'eshell)
(require 'em-hist)
(require 'em-glob)
(require 'em-prompt)
(require 'em-cmpl)

(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun bk/eshell-prompt-function ()
  "My eshell prompt function."
  (concat " λ "))

(add-hook 'eshell-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(setq eshell-save-history-on-exit t
      eshell-hist-ignoredups t
      eshell-glob-case-insensitive t
      eshell-error-if-no-glob t
      eshell-highlight-prompt nil
      eshell-prefer-lisp-functions t
      eshell-prompt-function #'bk/eshell-prompt-function
      eshell-cmpl-cycle-completions nil)

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (eshell/alias "e" "find-file $1")
	    (eshell/alias "emacs" "find-file $1")
	    (eshell/alias "ee" "find-file-other-window $1")
	    (eshell/alias "gsm" "git submodule add $1")
	    (eshell/alias "d" "dired $1")
	    (eshell/alias "ll" "ls -l")))

(add-hook 'eshell-mode-hook #'abbrev-mode)

(provide 'setup-eshell)
;;; setup-eshell.el ends here
