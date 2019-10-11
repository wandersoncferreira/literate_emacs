;;; setup-latex --- Latex editting
;;; Commentary:

;; The best option for serious writting.

;;; Code:

(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-view-program-selection '())
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))

(use-package reftex
  :ensure t
  :config
  (setq reftex-cite-prompt-optional-args t))

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil
      TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(with-eval-after-load 'tex
  (add-to-list 'safe-local-variable-values
               '(TeX-command-extra-options . "-shell-escape")))

(defun bk/latex-template-article ()
  "Personal LaTeX template."
  (interactive)
  (insert "\\documentclass[11pt]{article}\n")
  (insert "\\usepackage[brazilian]{babel}\n")
  (insert "\\usepackage[utf8]{inputenc}\n")
  (insert "\\usepackage[T1]{fontenc}\n")
  (insert "\\usepackage{graphicx}\n")
  (insert "\\usepackage{cite}\n")
  (insert "\\title{INSERT YOUR TITLE HERE}\n")
  (insert "\\author{Wanderson Ferreira}\n")
  (insert "\\date{\\today}\n")
  (insert "\\begin{document}\n")
  (insert "\\maketitle\n\n\n")
  (insert "\\bibliographystyle{plain}\n")
  (insert "\\bibliography{NAME-BIB-FILE}\n")
  (insert "\\end{document}\n\n")
  (insert "This text will not show up in the output"))

(defun bk/latex-template-beamer ()
  "Personal LaTeX template for beamer presentations."
  (interactive)
  (insert "\\documentclass{beamer}\n")
  (insert "\\mode<presentation>\n")
  (insert " {\n")
  (insert "\\setheme{Madrid}\n")
  (insert "\\secolortheme{default}\n")
  (insert "\\sefonttheme{serif}\n")
  (insert "\\setbeamertemplate{navigation symbols}{}\n")
  (insert "\\setbeamertemplate{caption}[numbered]\n")
  (insert " }\n")
  (insert "\\usepackage[english]{babel}\n")
  (insert "\\usepackage[utf8x]{inputenc}\n")
  (insert "\\usepackage{chemfig}\n")
  (insert "\\usepackage[version=3]{mhchem}\n")
  (insert "\n")
  (insert "\\title[teste]{Descricao}}\n")
  (insert "\\author{Wanderson Ferreira}\n")
  (insert "\\institute{bartuka.com}\n")
  (insert "\\date{\\today}\n")
  (insert "\n")
  (insert "\\begin{document}\n")
  (insert "\n")
  (insert "\\begin{frame}\n")
  (insert "\\titlepage\n")
  (insert "\\end{frame}\n")
  (insert "\n")
  (insert "\\begin{frame}{Outline}\n")
  (insert "\\tableofcontents\n")
  (insert "\\end{frame}\n")
  (insert "\\end{document}\n"))


(use-package pdf-tools
  :ensure t
  :init
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(provide 'setup-latex)
;;; setup-latex.el ends here
