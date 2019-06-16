;;; setup-pdf --- PDF Tools
;;; Commentary:

;; 

;;; Code:

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(provide 'setup-pdf)
;;; setup-pdf.el ends here
