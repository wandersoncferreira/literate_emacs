;;; setup-latex --- Latex editting
;;; Commentary:

;; The best option for serious writting.

;;; Code:

(bk/install-maybe 'auctex)

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil
      TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq teste-tex "ativo")

(provide 'setup-latex)
;;; setup-latex.el ends here
