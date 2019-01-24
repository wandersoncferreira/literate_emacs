;;; setup-sml --- Standard MetaLanguage of New Jersey
;;; Commentary:

;;

;;; Code:

(use-package sml-mode :ensure t)
(use-package geiser
  :ensure t
  :config
  (setq geiser-default-implementation "racket"))

(provide 'setup-sml)
;;; setup-sml.el ends here
