;;; setup-maintainer --- All my custom packages
;;; Commentary:

;; Custom packages

;;; Code:

(bk/install-maybe 'helm)
(bk/install-maybe 'multi)

(autoload 'helm-spotify-plus "helm-spotify-plus" "Control spotify" t nil)

(provide 'setup-maintainer)
;;; setup-maintainer.el ends here
