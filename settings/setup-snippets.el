(bk/install-maybe-require 'yasnippet)
(bk/install-maybe-require 'yasnippet-snippets)
(bk/install-maybe-require 'clojure-snippets)

(yas-global-mode +1)
(diminish 'yas-minor-mode)

;; jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "M-i") 'yas-expand)

(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
(setq yas-verbosity 1)
(setq yas-wrap-around-region t)

(provide 'setup-snippets)
