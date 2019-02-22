;;; setup-maintainer --- All my custom packages
;;; Commentary:

;; Custom packages

;;; Code:


(use-package helm-spotify-plus
  :ensure t
  :demand t
  :bind
  (("C-c s s" . helm-spotify-plus)
   ("C-c s f" . helm-spotify-plus-next)
   ("C-c s b" . helm-spotify-plus-previous)
   ("C-c s p" . helm-spotify-plus-play)
   ("C-c s g" . helm-spotify-plus-pause)))

(provide 'setup-maintainer)
;;; setup-maintainer.el ends here
