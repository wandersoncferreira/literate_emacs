;;; Disable dynamic scopes ---  -*- lexical-binding: t -*- 
;;; misc --- my miscellaneous
;;; Commentary:
;;; Code:

;;; install only packages
(bk/install-maybe 'htmlize)
(bk/install-maybe 'restclient)
(bk/install-maybe 'yaml-mode)
(bk/install-maybe 'graphviz-dot-mode)
(bk/install-maybe 'quickrun)
(bk/install-maybe 'windresize)
(bk/install-maybe 'rotate)
(bk/install-maybe 'keycast)
(bk/install-maybe 'dotenv-mode)
(bk/install-maybe 'discover-my-major)

(use-package change-inner :ensure t)
(use-package wgrep :ensure t)
(use-package vlf :ensure t)
(use-package webpaste
  :ensure t
  :config
  (setq webpaste-provider-priority '("dpaste.de")))

(provide 'setup-misc)
;;; setup-misc.el ends here
