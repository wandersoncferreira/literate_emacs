
(defun eshell-clear-buffer ()
  "clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook '(lambda ()
			       (local-set-key (kbd "C-l") 'eshell-clear-buffer)))


;;; defaults
(setq eshell-save-history-on-exit t
      eshell-glob-case-insensitive t
      eshell-error-if-no-glob t
      eshell-cmpl-cycle-completions nil)


;; aliases
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (eshell/alias "e" "find-file $1")
	    (eshell/alias "emacs" "find-file $1")
	    (eshell/alias "ee" "find-file-other-window $1")
	    (eshell/alias "gsm" "git submodule add $1")
	    (eshell/alias "d" "dired $1")
	    (eshell/alias "ll" "ls -l")))

(provide 'setup-eshell)
