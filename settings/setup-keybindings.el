;;; highlight symbols
(global-set-key (kbd "M-n") 'highlight-symbol-at-point)
(global-set-key (kbd "M-p") 'unhighlight-regexp)

;; eshell
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-x \"") 'eshell)

;;; ibuffer instead of list buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; quickly jump in document with ace-jump-mode
(define-key global-map (kbd "C-'") 'ace-jump-mode)

;; M-i for back-to-indetation
(global-set-key (kbd "C-a") 'back-to-indentation)

;;; Transpose stuff with M-t
(global-unset-key (kbd "M-t"))
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexp)
(global-set-key (kbd "M-t p") 'transpose-lines)

;;; zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") (lambda (char)
			      (interactive "cZap up to char backwards: " (zap-up-to-char -1 char))))

;;; iy-go-to-char - like f in Vim
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
(global-set-key (kbd "s-m") 'jump-char-backward)

;;; pop to mark
(global-set-key (kbd "C-x p") 'pop-to-mark-command)

;; jump to a definition in current file
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;;; toggle two mos recentf buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)

(global-set-key (kbd "s-y") 'bury-buffer)

;;; prodigy
(global-set-key (kbd "C-x M-m") 'prodigy)

;;; window switching
(windmove-default-keybindings)

;;; browse kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;;; magit status
(global-set-key (kbd "C-c m s") 'magit-status)

(provide 'setup-keybindings)
