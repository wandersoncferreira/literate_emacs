;;; setup-grep --- Grep
;;; Commentary:

;; Search for your patterns with ease

;;; Code:

(require 'grep)

(setq grep-highlight-matches t
      grep-scroll-output t)

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "elpa")
     (add-to-list 'grep-find-ignored-directories "vendor")
     (add-to-list 'grep-find-ignored-directories "node_modules")

     (define-key grep-mode-map "q" 'rgrep-quit-window)
     (define-key grep-mode-map (kbd "C-<return>") 'rgrep-goto-file-and-close-rgrep)
     (define-key grep-mode-map (kbd "C-x C-s") 'wgrep-save-all-buffers)))

(use-package ag
  :ensure t)

(provide 'setup-grep)
;;; setup-grep.el ends here
