;;; setup-misc --- Miscellaneous
;;; Commentary:
;;; Code:

;;; install only packages
(use-package htmlize :ensure t)
(use-package restclient :ensure t)
(use-package yaml-mode :ensure t)
(use-package graphviz-dot-mode :ensure t)
(use-package quickrun :ensure t)
(use-package windresize :ensure t)
(use-package rotate :ensure t)
(use-package discover-my-major :ensure t)
(use-package plantuml-mode :ensure t)
(use-package change-inner :ensure t)
(use-package wgrep :ensure t)
(use-package vlf :ensure t)
(use-package neotree :ensure t)

(use-package webpaste
  :ensure t
  :config
  (setq webpaste-provider-priority '("dpaste.de")))

(use-package pdf-tools
  :ensure t
  :init
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package sqlformat
  :ensure t
  :init
  (setq sqlformat-command 'pgformatter))

(provide 'setup-misc)
;;; setup-misc.el ends here
