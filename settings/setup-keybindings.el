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
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 U") 'winner-redo)
(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp)
(global-set-key (kbd "C-h C-m") 'discover-my-major)


;;; C-x {letter} are reserved for Emacs native essentials: buffers, window, frame, file, directory, etc.
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x C-p") 'pop-global-mark)

;;; C-{symbols} are reserved for user
(global-unset-key (kbd "C-\\"))

;; (global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)
(global-set-key (kbd "C-;") 'flyspell-auto-correct-previous-word)
(global-set-key (kbd "C-*") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-\\") 'ace-jump-mode)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;;; C-c {letter} are reserved for user
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'stesla-rotate-buffers)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c r") 'restart-emacs)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c g") 'hydra-magit/body)
(global-set-key (kbd "C-c m s") 'helm-spotify-plus)
(global-set-key (kbd "C-c m f") 'helm-spotify-plus-next)
(global-set-key (kbd "C-c m b") 'helm-spotify-plus-previous)
(global-set-key (kbd "C-c m p") 'helm-spotify-plus-play)
(global-set-key (kbd "C-c m g") 'helm-spotify-plus-pause)
(global-set-key (kbd "C-c p") 'hydra-projectile/body)

;;; C-c C-{letter} are reserved for major mode
(global-set-key (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "C-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c C-i") 'bk/ido-menu)
(global-set-key (kbd "C-c C-k") 'bk/eval-buffer)
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "C-c C-q") 'quickrun)
(global-set-key (kbd "C-c C-r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c C-\"") 'quick-switch-buffer)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;; C-M <letter>
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;;; ---------- function keys --------------
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "<f7>") 'sanityinc/split-window)

;;;;    -------- Meta based keys  -----------

;;; M-{letter} bindings
;; (global-set-key (kbd "M-/") 'hippie-expand)

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

;;; show time in modeline when using Emacs in fullscreen [lgmoneda]
(global-set-key (kbd "<f9>") (lambda () (interactive)
                               (toggle-frame-fullscreen)
                               (sit-for 1)
                               (if (eq (cdr (assoc 'fullscreen (frame-parameters))) 'fullboth)
                                   (display-time-mode 1)
                                 (display-time-mode 0))))

;; registers
(set-register ?p '(file . "~/practice.org"))
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?i '(file . "~/notes/gtd/inbox.org"))


(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
