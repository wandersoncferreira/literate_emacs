;;; keybindings --- my customized keybindings
;;; Commentary:
;;
;; Organizing the Emacs keybindings to make it feel sane
;; 
;;; Code:

;;;;    -------- Command based keys -----------

;;; Remapping of original commands
(global-set-key (kbd "C-a") 'bk/back-to-indentation-or-beginning)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-z") 'bury-buffer)
(global-set-key (kbd "C-x f") 'ido-recentf-open)
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 U") 'winner-redo)
(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp)
(global-set-key (kbd "C-h C-m") 'discover-my-major)


;;; C-x {letter} are reserved for Emacs native essentials: buffers, window, frame, file, directory, etc.
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-m") 'amx)
(global-set-key (kbd "C-x C-p") 'pop-global-mark)
(global-set-key (kbd "C-x C-/") 'save-buffer)

;;; C-{symbols} are reserved for user
(global-unset-key (kbd "C-\\"))

(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)
(global-set-key (kbd "C-;") 'flyspell-auto-correct-previous-word)
(global-set-key (kbd "C-*") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "<C-dead-acute>") 'ace-jump-mode)
(global-set-key (kbd "C-\\") 'push-mark-no-activate)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;;; C-c {letter} are reserved for user
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'bk/scratch-buffer)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c g") 'hydra-magit/body)
(global-set-key (kbd "C-c m s") 'helm-spotify-plus)
(global-set-key (kbd "C-c m f") 'helm-spotify-plus-next)
(global-set-key (kbd "C-c m b") 'helm-spotify-plus-previous)
(global-set-key (kbd "C-c m p") 'helm-spotify-plus-play)
(global-set-key (kbd "C-c m g") 'helm-spotify-plus-pause)
(global-set-key (kbd "C-c p") 'hydra-projectile/body)
(global-set-key (kbd "C-c s") 'hydra-system/body)
(global-set-key (kbd "C-c w") 'google-this-search)

;;; C-c C-{letter} are reserved for major mode
(global-set-key (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "C-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c C-i") 'bk/ido-menu)
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "C-c C-q") 'quickrun)
(global-set-key (kbd "C-c C-r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c C-\"") 'quick-switch-buffer)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;; C-M <letter>
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;;; ---------- function keys --------------
(global-set-key (kbd "<f8>") 'neotree-toggle)

;;;;    -------- Meta based keys  -----------

;;; M-{letter} bindings
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-c") #'fix-word-capitalize)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-l") #'fix-word-downcase)
(global-set-key (kbd "M-n") 'highlight-symbol-at-point)
(global-set-key (kbd "M-o") 'change-outer)
(global-set-key (kbd "M-p") 'unhighlight-regexp)
(global-set-key (kbd "M-u") #'fix-word-upcase)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-unset-key (kbd "M-t"))
(global-set-key (kbd "M-g g") 'goto-line-with-feedback)
(global-set-key (kbd "M-s f") 'find-name-dired)
(global-set-key (kbd "M-s l") 'sort-lines)
(global-set-key (kbd "M-s o") 'bk/improve-occur)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-lines)

;;; mac os
(global-set-key (quote [M-f10]) 'toggle-frame-fullscreen)

;;; occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;;; zap to char
(global-set-key (kbd "s-z") (lambda (char)
                              (interactive "cZap up to char backwards: " (zap-up-to-char -1 char))))

;; registers
(set-register ?b '(file . "~/books.org"))
(set-register ?h '(file . "~/emacs-notes/7habits-effective-editing.org"))

;;; move lines
(bk/install-maybe 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [C-M-down] 'md/duplicate-down)
(global-set-key [C-M-up] 'md/duplicate-up)

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
