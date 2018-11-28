;;; highlight symbols
(global-set-key (kbd "M-n") 'highlight-symbol-at-point)
(global-set-key (kbd "M-p") 'unhighlight-regexp)

;;; completions
(global-set-key (kbd "C-,") 'complete-symbol)

;;; multiple cursors
(bk/install-maybe-require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; from active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;; dired
(global-set-key (kbd "M-s f") 'find-name-dired)
(global-set-key (kbd "C-x C-j") 'dired-jump)

;;; mac os
(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "s--") 'negative-argument)
(global-set-key (quote [M-f10]) 'toggle-frame-fullscreen)
(dotimes (v 5)
  (global-set-key (read-kbd-macro (format "s-%d" v)) 'digit-argument))

;;; occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)
(global-set-key (kbd "M-s o") 'bk/improve-occur)

;;; M-x
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; backspace key
(global-set-key [(control ?h)] 'delete-backward-char)

;;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "s-/") 'hippie-expand)

;;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;;; isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;;; open new lines
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;;; help command
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "<f1> a") 'apropos)

;; eshell
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-x \"") 'eshell)

;;; ido
(global-set-key (kbd "C-x f") 'ido-recentf-open)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;;; quickly jump in document with ace-jump-mode
(define-key global-map (kbd "C-'") 'ace-jump-mode)

;;; Transpose stuff with M-t
(global-unset-key (kbd "M-t"))
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-lines)

;;; zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") (lambda (char)
                              (interactive "cZap up to char backwards: " (zap-up-to-char -1 char))))

;;; iy-go-to-char - like f in Vim
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;;; buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)
(global-set-key (kbd "s-y") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c b") 'bk/scratch-buffer)

;;; prodigy
(global-set-key (kbd "C-x M-m") 'prodigy)

;;; window switching
(windmove-default-keybindings)

;;; browse kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;;; magit status
(global-set-key (kbd "C-c m s") 'magit-status)

;; misc
(global-set-key (kbd "M-s l") 'sort-lines)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)

;;; move lines
(bk/install-maybe-require 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [C-M-down] 'md/duplicate-down)
(global-set-key [C-M-up] 'md/duplicate-up)

(provide 'setup-keybindings)