;;; setup-eshell --- Shell
;;; Commentary:

;; SHELL

;;; Code:

(require 'eshell)

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
	    (eshell/alias "d" "dired $1")))

(add-hook 'eshell-mode-hook #'abbrev-mode)

(provide 'setup-eshell)
;;; setup-eshell.el ends here
