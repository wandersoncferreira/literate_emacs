;;; setup-eshell --- Shell
;;; Commentary:

;; SHELL

;;; Code:

(bk/install-maybe 'shell-command)


(require 'eshell)
(require 'shell-command)
(require 'bash-completion)

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

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")

(add-hook 'shell-dynamic-complete-functions
	  'bash-completion-dynamic-complete)
(add-hook 'shell-command-completion-function
	  'bash-completion-dynamic-complete)

(shell-command-completion-mode)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "To kill buffer if process if dead, ARG."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))
(add-hook 'shell-mode-hook
	  (lambda ()
	    (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(provide 'setup-eshell)
;;; setup-eshell.el ends here
