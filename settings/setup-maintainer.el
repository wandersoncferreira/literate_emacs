;;; setup-maintainer --- All my custom packages
;;; Commentary:

;; Custom packages

;;; Code:

(bk/install-maybe 'helm)
(bk/install-maybe 'multi)

(autoload 'helm-spotify-plus "helm-spotify-plus" "Control spotify" t nil)
(global-set-key (kbd "C-c s s") 'helm-spotify-plus)
(global-set-key (kbd "C-c s f") 'helm-spotify-plus-next)
(global-set-key (kbd "C-c s b") 'helm-spotify-plus-previous)
(global-set-key (kbd "C-c s p") 'helm-spotify-plus-play)
(global-set-key (kbd "C-c s g") 'helm-spotify-plus-pause)

(provide 'setup-maintainer)
;;; setup-maintainer.el ends here
