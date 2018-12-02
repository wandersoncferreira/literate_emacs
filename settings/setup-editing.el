;;; editing.el --- help me improve
;;; Commentary:
;;; Code:

;;; expand region
(bk/install-maybe-require 'expand-region)

;; don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;;; navigate by char
(bk/install-maybe-require 'jump-char)
(bk/install-maybe-require 'ace-jump-mode)

;; kill ring
(bk/install-maybe-require 'browse-kill-ring)
(require 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(setq browse-kill-ring-quit-action 'save-and-restore)

(eval-after-load 'page-break-lines
  '(progn
     (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
     (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
     (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous)))

(provide 'setup-editing)
;;; setup-editing.el ends here
